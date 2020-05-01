# -*- mode: ruby -*-
# vi: set ft=ruby :
Vagrant.configure("2") do |config|
  config.vm.box = "debian/testing64"

  # Install ansible locally
  config.vm.provision "shell", inline: <<-SHELL
    DEBIAN_FRONTEND=noninteractive apt-get install -y ansible
  SHELL

  config.vm.provision "ansible_local" do |ansible|
    ansible.playbook       = "playbook/main.yml"
    ansible.inventory_path = "vagrant_inventory"
    ansible.limit          = "all"
    # ansible.verbose        = true
  end
end
