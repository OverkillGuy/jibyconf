# -*- mode: ruby -*-
# vi: set ft=ruby :
Vagrant.configure("2") do |config|
  config.vm.box = "debian/testing64"

  config.vm.provision "shell", inline: <<-SHELL
    apt-get update
    DEBIAN_FRONTEND=noninteractive apt-get install -y ansible
  SHELL

  config.vm.provision "ansible_local" do |ansible|
    ansible.playbook       = "playbook.yml"
    ansible.inventory_path = "inventory"
    ansible.limit          = "all" # or only "nodes" group, etc.
    ansible.become         = true
    # ansible.verbose        = true
  end
end
