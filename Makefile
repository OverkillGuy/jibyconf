SKIP_TAGS=slow,x11,plantuml
ANSIBLE_OPTS=--skip-tags ${SKIP_TAGS}

export PATH := ${HOME}/.local/bin/:$(PATH)

# Wipe machine and restart from scratch using vagrant-provided
# ansible provisioning
test-vagrant: delete up provision-ansible

# Wipe machine and restart from scratch using SSH-only ansible
# provisioning (no vagrant)
test-ansible: delete up provision ansible

# Wipe machine and restart from scratch using ansible-pull via git
# clone (called by vagrant)
test-pull: delete up provision-ansible-pull


up:
	vagrant up

down:
	vagrant halt

PROVISION_OPTS=

# Default: just the shell provisioner
provision:
	vagrant provision ${PROVISION_OPTS}


provision-shell:  PROVISION_OPTS += --provision-with shell
provision-shell: provision

provision-ansible:  PROVISION_OPTS += --provision-with vagrant-ansible-local
provision-ansible: provision

provision-ansible-pull:  PROVISION_OPTS += --provision-with ansible-pull
provision-ansible-pull: provision

sync:
	vagrant rsync

delete:
	vagrant destroy -f

ansible:
	ansible-playbook -i vagrant_inventory playbook/main.yml --limit debby  ${ANSIBLE_OPTS}



