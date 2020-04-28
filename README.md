# Debian VM playbook

Generate a new Debian VM via Vagrant, customized by Ansible playbook.

Playbook tailored to personal laptop usage.

## Dependencies
- [Vagrant](https://vagrantup.com) for the VM launch
- `debian/testing64` vagrant basebox (bullseye)
- Ansible for provisioning (installed by Vagrant on the VM)


## Installation

To launching this playbook, [install Vagrant](https://www.vagrantup.com/intro/getting-started/install.html).
You'll also need a Vagrant provider that works with the basebox. We recommend [virtualbox](https://www.vagrantup.com/docs/virtualbox/).

Launching the VM the first time is a matter of running

	vagrant up

This will:
- Download the box if needed
- Launch the VM, creating it if doesn't exist
- Provision the machine if needed (first time)

## Usage

The setup is a vagrant machine. Read the [vagrant CLI docs](https://www.vagrantup.com/docs/cli/) for details.
The most common operation is to launch the machine via `vagrant up`, and ssh to it via `vagrant ssh`.

If the machine is to be manipulated outside vagrant (via Virtualbox or directly over SSH), consider using [vagrant ssh-config](https://www.vagrantup.com/docs/cli/ssh_config.html) to simplify future connections via standalone SSH.

Rebuild the machine by burning it down, reproducing it from basebox:

	# WARNING: This destroys your copy of the machine, rebuilding it from scratch
	vagrant destroy -f && vagrant up --provision

Users comfortable with Ansible should look at the playbook as mostly separate from Vagrant using it.




