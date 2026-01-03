//! Lightweight system/environment helpers used by CLI tooling.
//!
//! Keep this module dependency-free (std only) so binaries and `xtask` can share logic without
//! duplicating platform quirks.

use std::path::Path;

/// Determine a conservative CPU budget for default parallelism.
///
/// This is primarily used by pageset tooling to pick sane defaults for:
/// - the number of worker processes (`--jobs`)
/// - per-worker Rayon thread counts (`RAYON_NUM_THREADS`)
///
/// On Linux, the budget is clamped by cgroup CPU quotas when available so container/CI runs don't
/// oversubscribe the host.
pub fn cpu_budget() -> usize {
  let mut cpus = std::thread::available_parallelism().map_or(1, |n| n.get()).max(1);
  #[cfg(target_os = "linux")]
  {
    if let Some(quota) = linux_cgroup_cpu_quota() {
      cpus = cpus.min(quota);
    }
  }
  cpus.max(1)
}

#[cfg(target_os = "linux")]
fn ceil_div_u64(numer: u64, denom: u64) -> u64 {
  if denom == 0 {
    return 0;
  }
  numer
    .saturating_add(denom.saturating_sub(1))
    .saturating_div(denom)
}

#[cfg(target_os = "linux")]
fn parse_cgroup_v2_cpu_max(contents: &str) -> Option<usize> {
  let mut it = contents.split_whitespace();
  let quota_raw = it.next()?;
  let period_raw = it.next()?;
  if quota_raw == "max" {
    return None;
  }
  let quota = quota_raw.parse::<u64>().ok()?;
  let period = period_raw.parse::<u64>().ok()?;
  if quota == 0 || period == 0 {
    return None;
  }
  let cpus = ceil_div_u64(quota, period).max(1);
  Some(cpus as usize)
}

#[cfg(target_os = "linux")]
fn parse_cgroup_v1_cpu_quota(quota_raw: &str, period_raw: &str) -> Option<usize> {
  let quota = quota_raw.trim().parse::<i64>().ok()?;
  let period = period_raw.trim().parse::<i64>().ok()?;
  if quota <= 0 || period <= 0 {
    return None;
  }
  let cpus = ceil_div_u64(quota as u64, period as u64).max(1);
  Some(cpus as usize)
}

#[cfg(target_os = "linux")]
fn parse_proc_self_cgroup_v2_path(contents: &str) -> Option<&str> {
  contents.lines().find_map(|line| {
    let mut it = line.splitn(3, ':');
    let id = it.next()?;
    let controllers = it.next()?;
    let path = it.next()?;
    if id == "0" && controllers.is_empty() {
      Some(path)
    } else {
      None
    }
  })
}

#[cfg(target_os = "linux")]
fn parse_proc_self_cgroup_v1_controller_path<'a>(
  contents: &'a str,
  controller: &str,
) -> Option<&'a str> {
  contents.lines().find_map(|line| {
    let mut it = line.splitn(3, ':');
    let _id = it.next()?;
    let controllers = it.next()?;
    let path = it.next()?;
    if controllers.split(',').any(|candidate| candidate == controller) {
      Some(path)
    } else {
      None
    }
  })
}

#[cfg(target_os = "linux")]
fn cgroup_v2_cpu_quota(root: &Path, proc_cgroup: Option<&str>) -> Option<usize> {
  let relative = proc_cgroup
    .and_then(parse_proc_self_cgroup_v2_path)
    .unwrap_or("/");
  let mut dir = root.join(relative.trim_start_matches('/'));
  if dir.as_os_str().is_empty() {
    dir = root.to_path_buf();
  }
  let mut quota: Option<usize> = None;
  loop {
    if let Ok(contents) = std::fs::read_to_string(dir.join("cpu.max")) {
      if let Some(cpus) = parse_cgroup_v2_cpu_max(&contents) {
        quota = Some(quota.map_or(cpus, |prev| prev.min(cpus)));
      }
    }
    if dir == root {
      break;
    }
    let Some(parent) = dir.parent() else {
      break;
    };
    dir = parent.to_path_buf();
  }
  quota
}

#[cfg(target_os = "linux")]
fn cgroup_v1_cpu_quota(mountpoint: &Path, proc_cgroup: Option<&str>) -> Option<usize> {
  let relative = proc_cgroup
    .and_then(|raw| parse_proc_self_cgroup_v1_controller_path(raw, "cpu"))
    .unwrap_or("/");
  let mut dir = mountpoint.join(relative.trim_start_matches('/'));
  if dir.as_os_str().is_empty() {
    dir = mountpoint.to_path_buf();
  }
  let mut quota: Option<usize> = None;
  loop {
    let quota_path = dir.join("cpu.cfs_quota_us");
    let period_path = dir.join("cpu.cfs_period_us");
    if let (Ok(quota_raw), Ok(period_raw)) =
      (std::fs::read_to_string(quota_path), std::fs::read_to_string(period_path))
    {
      if let Some(cpus) = parse_cgroup_v1_cpu_quota(&quota_raw, &period_raw) {
        quota = Some(quota.map_or(cpus, |prev| prev.min(cpus)));
      }
    }
    if dir == mountpoint {
      break;
    }
    let Some(parent) = dir.parent() else {
      break;
    };
    dir = parent.to_path_buf();
  }
  quota
}

#[cfg(target_os = "linux")]
fn linux_cgroup_cpu_quota() -> Option<usize> {
  // cgroup quota enforcement is hierarchical; the effective CPU ceiling is the minimum quota
  // observed from the current cgroup up through its ancestors.
  let proc_cgroup = std::fs::read_to_string("/proc/self/cgroup").ok();

  let cgroup_root = Path::new("/sys/fs/cgroup");
  if let Some(quota) = cgroup_v2_cpu_quota(cgroup_root, proc_cgroup.as_deref()) {
    return Some(quota);
  }

  // cgroup v1 might mount the CPU controller as `/sys/fs/cgroup/cpu` (or combined with cpuacct).
  for mount in ["/sys/fs/cgroup/cpu", "/sys/fs/cgroup/cpu,cpuacct"] {
    let mountpoint = Path::new(mount);
    if let Some(quota) = cgroup_v1_cpu_quota(mountpoint, proc_cgroup.as_deref()) {
      return Some(quota);
    }
  }

  None
}

#[cfg(test)]
mod tests {
  use super::*;

  #[cfg(target_os = "linux")]
  #[test]
  fn parse_cgroup_v2_cpu_max_values() {
    assert_eq!(parse_cgroup_v2_cpu_max("max 100000"), None);
    assert_eq!(parse_cgroup_v2_cpu_max("100000 100000"), Some(1));
    assert_eq!(parse_cgroup_v2_cpu_max("200000 100000"), Some(2));
    assert_eq!(parse_cgroup_v2_cpu_max("50000 100000"), Some(1));
    assert_eq!(parse_cgroup_v2_cpu_max("0 100000"), None);
    assert_eq!(parse_cgroup_v2_cpu_max("100000 0"), None);
    assert_eq!(parse_cgroup_v2_cpu_max("oops"), None);
  }

  #[cfg(target_os = "linux")]
  #[test]
  fn parse_cgroup_v1_cpu_quota_values() {
    assert_eq!(parse_cgroup_v1_cpu_quota("-1", "100000"), None);
    assert_eq!(parse_cgroup_v1_cpu_quota("0", "100000"), None);
    assert_eq!(parse_cgroup_v1_cpu_quota("100000", "100000"), Some(1));
    assert_eq!(parse_cgroup_v1_cpu_quota("200000", "100000"), Some(2));
    assert_eq!(parse_cgroup_v1_cpu_quota("50000", "100000"), Some(1));
    assert_eq!(parse_cgroup_v1_cpu_quota("100000", "0"), None);
    assert_eq!(parse_cgroup_v1_cpu_quota("oops", "100000"), None);
  }

  #[cfg(target_os = "linux")]
  #[test]
  fn cgroup_v2_cpu_quota_walks_ancestors() {
    use std::fs;
    let dir = tempfile::tempdir().expect("tempdir");
    let root = dir.path();

    fs::write(root.join("cpu.max"), "max 100000").expect("write cpu.max");
    fs::create_dir_all(root.join("parent/child")).expect("mkdir");
    fs::write(root.join("parent/cpu.max"), "200000 100000").expect("write parent cpu.max");
    fs::write(root.join("parent/child/cpu.max"), "max 100000").expect("write child cpu.max");

    let proc_cgroup = "0::/parent/child\n";
    assert_eq!(cgroup_v2_cpu_quota(root, Some(proc_cgroup)), Some(2));
  }

  #[cfg(target_os = "linux")]
  #[test]
  fn cgroup_v1_cpu_quota_walks_ancestors() {
    use std::fs;
    let dir = tempfile::tempdir().expect("tempdir");
    let root = dir.path();

    fs::write(root.join("cpu.cfs_quota_us"), "-1").expect("write quota");
    fs::write(root.join("cpu.cfs_period_us"), "100000").expect("write period");
    fs::create_dir_all(root.join("parent/child")).expect("mkdir");
    fs::write(root.join("parent/cpu.cfs_quota_us"), "300000").expect("write parent quota");
    fs::write(root.join("parent/cpu.cfs_period_us"), "100000").expect("write parent period");
    fs::write(root.join("parent/child/cpu.cfs_quota_us"), "200000").expect("write child quota");
    fs::write(root.join("parent/child/cpu.cfs_period_us"), "100000").expect("write child period");

    let proc_cgroup = "2:cpu:/parent/child\n";
    // Effective quota should be the minimum across ancestors (2 CPUs).
    assert_eq!(cgroup_v1_cpu_quota(root, Some(proc_cgroup)), Some(2));
  }
}

