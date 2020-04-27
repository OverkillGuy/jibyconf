# -*- mode: ruby -*-
# vi: set ft=ruby :
Vagrant.configure("2") do |config|
  config.vm.box = "debian/testing64"

  config.vm.provision "shell", inline: <<-SHELL
    apt-get update
    apt-get install -y ansible
  SHELL

  config.vm.provision "ansible_local" do |ansible|
    ansible.playbook = "playbook.yml"
    ansible.install = false
  end
end
