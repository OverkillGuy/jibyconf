SKIP_TAGS=slow,x11,plantuml
ANSIBLE_OPTS=--skip-tags ${SKIP_TAGS}

export PATH := ${HOME}/.local/bin/:$(PATH)

# Wipe machine and restart from scratch using vagrant-provided
# ansible provisioning
test-vagrant: delete up

# Wipe machine and restart from scratch using SSH-only ansible
# provisioning (no vagrant)
test-ansible: delete up-noansible ansible

# Wipe machine and restart from scratch using ansible-pull via git
# clone (called by vagrant)
test-pull: delete up up-noansible provision-ansible-pull

UP_ARGS=

up:
	vagrant up ${UP_ARGS}

# Disable ansible provisioning
up-noansible: UP_ARGS += --provision-with shell
up-noansible: up

down:
	vagrant halt

PROVISION_OPTS=

# Default: shell + ansible provisioner
provision:
	vagrant provision ${PROVISION_OPTS}

provision-shell:  PROVISION_OPTS += --provision-with shell
provision-shell: provision

provision-ansible-pull:  PROVISION_OPTS += --provision-with ansible-pull
provision-ansible-pull: provision


sync:
	vagrant rsync

delete:
	vagrant destroy -f

playbook/roles:
	ansible-galaxy install -r requirements.yml -p playbook/roles/

ansible:
	ansible-playbook -i vagrant_inventory playbook/main.yml --limit debby  ${ANSIBLE_OPTS}

list-tags:
	ansible-playbook playbook/main.yml --list-tasks

