#!/usr/bin/env python3
import argparse
import os
import subprocess
import sys
import time
import signal
from pathlib import Path

INTERVAL = 0.5


def gather_files(root: Path, exts, ignore_dirs):
    all_files = {}
    ignore_dirs_abs = [str(Path(d).resolve()) for d in ignore_dirs]
    for dirpath, dirnames, filenames in os.walk(root):
        if any(str(Path(dirpath).resolve()).startswith(ig) for ig in ignore_dirs_abs):
            continue
        for f in filenames:
            if any(f.endswith(ext) for ext in exts):
                fp = Path(dirpath) / f
                try:
                    all_files[str(fp.resolve())] = fp.stat().st_mtime
                except Exception:
                    pass
    return all_files


def kill_proc_tree(proc):
    try:
        if proc.poll() is not None:
            return
        if sys.platform.startswith("win"):
            subprocess.run(
                ["taskkill", "/F", "/T", "/PID", str(proc.pid)],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
        else:
            try:
                pgid = os.getpgid(proc.pid)
                os.killpg(pgid, signal.SIGTERM)
            except Exception:
                try:
                    proc.terminate()
                except Exception:
                    pass
        try:
            proc.wait(timeout=3)
        except Exception:
            try:
                proc.kill()
            except Exception:
                pass
    except Exception:
        pass


def main():
    parser = argparse.ArgumentParser(description="Simple polling-based watcher")
    parser.add_argument("--root", default=".", help="Root director")
    parser.add_argument("--ext", nargs="+", required=True, help="File extensions")
    parser.add_argument("--ignore", nargs="*", default=[], help="Directories to ignore")
    parser.add_argument(
        "--cmd", nargs=argparse.REMAINDER, required=True, help="Command to run"
    )
    args = parser.parse_args()

    root_path = Path(args.root).resolve()
    if not root_path.exists():
        print(f"❌ Root path not found: {root_path}")
        sys.exit(1)

    print(f"📂 Watching: {root_path}")
    print(f"🎯 Extensions: {args.ext}")
    if args.ignore:
        print(f"🚫 Ignoring: {args.ignore}")
    print(f"🔩 Command: {' '.join(args.cmd)}")

    last_files = gather_files(root_path, args.ext, args.ignore)
    proc = None

    def run_cmd():
        nonlocal proc
        if proc and proc.poll() is None:
            kill_proc_tree(proc)
        cmd_str = " ".join(args.cmd)
        print(f"🔄 Restarting: {cmd_str}")
        if sys.platform.startswith("win"):
            proc = subprocess.Popen(
                cmd_str, shell=True, creationflags=subprocess.CREATE_NEW_PROCESS_GROUP
            )
        else:
            proc = subprocess.Popen(cmd_str, shell=True, preexec_fn=os.setsid)
        time.sleep(0.05)

    run_cmd()

    try:
        while True:
            time.sleep(INTERVAL)
            current_files = gather_files(root_path, args.ext, args.ignore)

            changed = False
            for f, mtime in current_files.items():
                if f not in last_files or last_files[f] < mtime:
                    changed = True
                    break
            if not changed:
                for f in last_files.keys():
                    if f not in current_files:
                        changed = True
                        break

            if changed:
                run_cmd()
                last_files = current_files

    except KeyboardInterrupt:
        print("\n🛑 Stopping watcher...")
        if proc:
            kill_proc_tree(proc)


if __name__ == "__main__":
    main()
