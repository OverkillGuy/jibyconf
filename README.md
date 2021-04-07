# Jiby's dev environment playbook

Configure a Debian machine for development using Ansible playbook.

Test it on a new Debian bullseye VM via Vagrant, auto-provisioning it.

Alternatively, a debian-based server deploying gitea via docker is
also available in the Vagrant file for more experimentation.


## Demo

[![asciicast](https://asciinema.org/a/RdQQQ4SyXSgNujDUOR7tutbm5.svg)](https://asciinema.org/a/RdQQQ4SyXSgNujDUOR7tutbm5)


## Dependencies
- [Ansible](https://ansible.com) for configuration
- [Vagrant](https://vagrantup.com) for testing in VM (installs ansible)
- [GNU Stow](https://www.gnu.org/software/stow/) for config files
- [Debian](https://debian.org) as operating system

## Usage as VM

To try it out in VM, [install Vagrant](https://www.vagrantup.com/intro/getting-started/install.html).
You'll also need a Vagrant provider that works with the basebox. We recommend [virtualbox](https://www.vagrantup.com/docs/virtualbox/).

Launching the VM the first time is a matter of running

	vagrant up
	# optionally specify "dev" or "server" to get only 1 machine

This will:
- Download the box if needed
- Launch the VM, creating it if doesn't exist
- Provision the machine if needed (first time)

Check on the machines status:

	vagrant status

This will reveal the two machines available

	Current machine states:

	dev                       running (virtualbox)
	server                    not created (virtualbox)

	This environment represents multiple VMs. The VMs are all listed
	above with their current state. For more information about a specific
	VM, run `vagrant status NAME`.

SSH to the development machine

	vagrant ssh dev

If the machine is to be manipulated outside vagrant (via Virtualbox or
directly over SSH), please use [vagrant ssh-config](https://www.vagrantup.com/docs/cli/ssh_config.html) to
simplify future connections via standalone SSH:

	vagrant ssh-config dev
	# edit the output to suit, and append to ~/.ssh/config
	# I configured ssh-config(5) to use "debby" alias
	ssh debby  # no need for vagrant anymore!

Read the [vagrant CLI docs](https://www.vagrantup.com/docs/cli/) for more commands and details.

To rebuild the machine from the ground up, burning it down and
reproducing it from basebox:

	# WARNING: This destroys your copy of the machines, starting over from scratch!
	vagrant destroy -f && vagrant up
	# conveniently aliased to
	make test-vagrant

Given a running VM, after updating the playbook, you might want to
re-run the provisioning without recreating the machine. I like the
following command:

	# apply provisioning again
	vagrant provision
	# aliased to
	make provision

Remember that the current folder is shared over to the machine in
`/vagrant`, and is synchronised with:

	# update the copy of folder in /vagrant
	vagrant rsync

## Via Ansible playbook

Users comfortable with Ansible should look at the playbook as mostly
separate from Vagrant using it, by disabling the automated ansible
provisioning step in Vagrantfile. Instead, use ansible as standalone
via SSH after the barebones vagrant provisioning:

	export ANSIBLE_FORCE_COLOR=true # optional: for colorful output
    ansible-playbook -i vagrant_inventory playbook/main.yml debby
	# aliased to
	make ansible

`debby` is the VM's hostname, and is the alias I set up manually in
`~/.ssh/config` via `vagrant ssh-config`, so that `ssh debby` works.

### Customizing image via Tags

To make the playbook flexible enough to work on many systems,
this playbook defines [Ansible Tags](https://docs.ansible.com/ansible/latest/user_guide/playbooks_tags.html)
for all its sub-targets, with the purpose of customizing the environment.
By default, all tags are used, but some likely are worth skipping for
your particular usecase. Skip tags by adding keyword after
ansible-playbook command:

	--skip-tags x11
    --skip-tags dev,slow,security
	# If using spaces: make sure to wrap tags in quote!
	--skip-tags "docs, slow"

See the available tags per tasks:

	ansible-playbook playbook/main.yml --list-tasks
	# aliased to
	make list-tags

Sample output:

	play #5 (all): Emacs and its config (via stow)	TAGS: [emacs]
	    tasks:
	      Install emacs (no X)	TAGS: [emacs]
	      Install emacs (X11) is installed	TAGS: [emacs, x11]
	      Install dependencies of pdf-tools package	TAGS: [emacs, x11]
	      Deploy the emacs-conf repo via stow	TAGS: [emacs]
	      Ensure emacs bootstrap script present	TAGS: [emacs]
	      Ensure emacs daemon service enabled	TAGS: [emacs]
	      Load bootstrap emacs-config script	TAGS: [emacs, slow]


## Via GNU Stow

Configuration files ("dotfiles") of this setup are managed using [GNU Stow](https://www.gnu.org/software/stow/),
which deploys "packages" using symlinks.

Using Stow makes it easy to keep one centralized repository of all
config files in version control, while deploying them far away in the
filesystem, as each package can be "stowed" individually. See stowed
packages in the `stow/` folder.

## Limitations

- Separation of Ansible and Vagrant steps not full
  - Some hardcoded vagrant-only paths
  - Not tested on non-Vagrantbox debian bullseye to compare
- Plantuml install step errors out due to sourceforge download issue (skip it)
- GNU Stow (shell) steps aren't idempotent by default
  - Breaking ansible idempotency workflow
  - Workaround of checking "creates" folders, tying playbook to stowed packages
- Mostly untested on GUI machine (always skipping x11 step, too slow and power hungry)
- Stow package contents needs work
  - Was previously my emacs configuration folder, ported as-is
  - Some config doesn't work on new machines (wrong username etc)
- Lack of customization options for stow config (could move to ansible templates...)

## Notes for self.

### Further reading
Compare with https://github.com/math0ne/dotfiles/, another Ansible + stow solution


Testing the merging of repos, from [SO](https://stackoverflow.com/a/14992078)

	git subtree add --prefix=stow file:////home/jiby/dev/conf/emacs-conf emacsconf-premerge


See also [more apps selfhosted](https://github.com/ReinerNippes/selfhosted_on_docker), [awesome-selfhost](https://github.com/awesome-selfhosted/awesome-selfhosted) and [awesome-syadmin](https://github.com/n1trux/awesome-sysadmin).


### Systemd user login session issue

Systemd services can't be symlinked, hardcopy instead see contradictory
discussion at https://github.com/systemd/systemd/issues/3660

From https://github.com/zoqaeski/systemd-user-units#update
> As of systemd-206 and higher, most of this fails to work as expected
> due to how loginctl creates user slices: user services run outside
> of the session, so NO session data is available to them.

Worked around for Emacs by using the service file shipped by package.
