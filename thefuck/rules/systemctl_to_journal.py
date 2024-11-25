# A rule for `thefuck` to run the suggested `journalctl` command
# when `systemctl` complains about a service failing to start.


def match(command):
    # Check if the command starts with `systemctl` and mentions `journalctl`
    return (
        (
            command.script_parts[0] == "systemctl"
            or (
                command.script_parts[0] == "sudo"
                and command.script_parts[1] == "systemctl"
            )
        )
        and "Job for" in command.output
        and ("failed" in command.output or "error code" in command.output)
        and "journalctl" in command.output
        and "-xeu" in command.output
    )


def get_new_command(command):
    # Extract the service name from the output to use in the journalctl command
    lines = command.output.splitlines()
    for line in lines:
        if "Job for" in line and ".service" in line:
            service_name = line.split()[2].replace(".service", "")
            if "--user" in command.output:
                return f"journalctl --user -xeu {service_name}"
            else:
                return f"journalctl -xeu {service_name}"
    return None  # No valid command found, but this should rarely happen if the match succeeds


priority = 900
