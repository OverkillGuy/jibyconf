SKIP_TAGS=slow,x11
ANSIBLE_OPTS= --skip-tags ${SKIP_TAGS}
# -t x11,docker,emacs --skip-tags plantuml

VAGRANT_ARGS=

# Wipe machine and restart from scratch using ansible on host (requires
# ansible on the host machine)
test-vagrant: delete up

# Wipe machine and restart from scratch using SSH-only ansible
# provisioning (no vagrant)
test-ansible: delete up-noansible ansible

# Wipe machine and restart from scratch using ansible-pull via git
# clone (called by vagrant)
test-pull: delete up up-noansible provision-ansible-pull

up:
	vagrant up ${VAGRANT_ARGS}

down:
	vagrant halt ${VAGRANT_ARGS}

PROVISION_OPTS=

# Default: shell + ansible provisioner
provision:
	vagrant provision ${VAGRANT_ARGS} ${PROVISION_OPTS}

provision-shell:  PROVISION_OPTS += --provision-with shell
provision-shell: provision

provision-ansible-pull:  PROVISION_OPTS += --provision-with ansible-pull
provision-ansible-pull: provision


sync:
	vagrant rsync ${VAGRANT_ARGS}

delete:
	vagrant destroy -f ${VAGRANT_ARGS}

playbook/roles:
	ansible-galaxy install -r requirements.yml -p playbook/roles/

ansible:
	ANSIBLE_FORCE_COLOR=true ansible-playbook -i vagrant_inventory playbook/main.yml ${ANSIBLE_OPTS}

ansible-server: VAGRANT_ARGS += server
ansible-server: up
	ANSIBLE_FORCE_COLOR=true ansible-playbook -i vagrant_inventory playbook/server/main.yml --limit servers ${ANSIBLE_OPTS}

list-tags:
	ansible-playbook playbook/main.yml --list-tasks

lint:
	pre-commit run --all --all-files
