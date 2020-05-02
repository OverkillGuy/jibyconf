# -*- mode: ruby -*-
# vi: set ft=ruby :
Vagrant.configure("2") do |config|
  config.vm.box = "debian/testing64"

  # Install ansible locally
  config.vm.provision "shell", inline: <<-SHELL
    DEBIAN_FRONTEND=noninteractive apt-get install -y ansible git
  SHELL

  # Old-style: Vagrant calls ansible implicitly
  config.vm.provision "ansible_local" do |ansible|
    ansible.playbook       = "playbook/main.yml"
    ansible.inventory_path = "vagrant_inventory"
    ansible.limit          = "all"
    # ansible.verbose        = true
  end

  # # New-style: ansible-pull to clone repo + apply it locally
  # # Closer to how machines would run this themselves
  # # Note: Deescalate to vagrant user to avoid default root user issues in playbook
  # config.vm.provision "shell", inline: <<-SHELL
  #    sudo -u vagrant PYTHONUNBUFFERED=1 ANSIBLE_FORCE_COLOR=true ansible-pull -U file:///vagrant/  -i vagrant_inventory -C pullable playbook/main.yml
  # SHELL
end
