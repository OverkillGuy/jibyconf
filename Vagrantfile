# -*- mode: ruby -*-
# vi: set ft=ruby :
Vagrant.configure("2") do |config|
  config.vm.box = "debian/testing64"
  config.vm.hostname = "debby"

  config.vm.provision "bootstrap-ansible", type: "shell" do |s|
    s.inline = "DEBIAN_FRONTEND=noninteractive apt-get install -y ansible git"
  end

  # Optional "vagrant-ansible-local" solution for provisioning
  config.vm.provision "vagrant-ansible-local", type: "ansible_local", run: "never" do |ansible|
    ansible.playbook       = "playbook/main.yml"
    ansible.inventory_path = "vagrant_inventory"
    ansible.skip_tags      = "x11,slow,plantuml,emacs,pipx"
    ansible.limit          = "localhost"
    # ansible.verbose        = true
  end

  # Alternative: ansible-pull to clone repo + apply it locally.
  # Closer to how machines would run this themselves.
  # Buffered output (cuts off), use PYTHON_UNBUFFERED=1, but performance drops.
  # Note: Deescalate to vagrant user to avoid default root user issues in playbook
  config.vm.provision "ansible-pull", type: "shell", run: "never" do |s|
    s.inline = <<-SHELL
     sudo -u vagrant ANSIBLE_FORCE_COLOR=true ansible-pull -U file:///vagrant/  -i vagrant_inventory -C tmp playbook/main.yml --skip-tags "x11,slow,plantuml,emacs,pipx"
     SHELL
  end
end
