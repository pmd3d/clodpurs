import { spawnSync } from "child_process";

export const runCommandImpl = (cmd) => (args) => () => {
  const result = spawnSync(cmd, args, { stdio: ["pipe", "inherit", "inherit"] });
  if (result.error) {
    throw result.error;
  }
  if (result.status !== 0) {
    throw new Error("Command failed: " + cmd);
  }
};

export const runCommandOutputImpl = (cmd) => (args) => () => {
  const result = spawnSync(cmd, args, { stdio: ["pipe", "pipe", "inherit"] });
  if (result.error) {
    throw result.error;
  }
  if (result.status !== 0) {
    throw new Error("Command failed: " + cmd);
  }
  return result.stdout.toString().trim();
};
