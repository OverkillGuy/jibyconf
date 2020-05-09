# -*- mode: ruby -*-
# vi: set ft=ruby :
Vagrant.configure("2") do |config|
  config.vm.define "dev", primary: true do |dev|
    dev.vm.box = "debian/testing64"
    dev.vm.hostname = "debby"
    # Optional-ish "vagrant-ansible-local" solution for provisioning
    dev.vm.provision "dev-ansible-local", type: "ansible_local" do |ansible|
      ansible.playbook       = "playbook/main.yml"
      ansible.inventory_path = "vagrant_inventory"
      ansible.skip_tags      = "x11,slow,plantuml"
      ansible.limit          = "localhost"
      # ansible.verbose        = true
    end
    # Alternative: ansible-pull to clone repo + apply it locally.
    # Closer to how machines would run this themselves.
    # Buffered output (cuts off), use PYTHON_UNBUFFERED=1, but performance drops.
    # Note: Deescalate to vagrant user to avoid default root user issues in playbook
    dev.vm.provision "ansible-pull", type: "shell", run: "never" do |s|
      s.inline = <<-SHELL
     sudo -u vagrant ansible-galaxy install -r /vagrant/requirements.yml -p roles/
     sudo -u vagrant ANSIBLE_FORCE_COLOR=true ansible-pull \
           -U file:///vagrant/  \
           -i vagrant_inventory \
           -C master \
           --limit 'localguy' \
           --skip-tags "hostrequired,x11,slow,plantuml"  \
           playbook/main.yml
     SHELL
    end
  end

  # A server on which to run the docker stuff
  config.vm.define "server" do |server|
    server.vm.box = "debian/buster64"
    server.vm.hostname = "sergei"
    server.vm.network "forwarded_port", guest: 3000, host: 3000, id: "Gitea HTTP"
    server.vm.network "forwarded_port", guest: 222, host: 2224, id: "Gitea SSH"

    server.vm.provision "server-ansible-local", type: "ansible_local" do |ansible|
      ansible.playbook       = "playbook/main.yml"
      ansible.inventory_path = "vagrant_inventory"
      ansible.tags           = "docker"
      ansible.limit          = "localhost"
      ansible.galaxy_role_file = "requirements.yml"
      # ansible.verbose        = true
    end
  end

  config.vm.provision "bootstrap-ansible", type: "shell" do |s|
    s.inline = "DEBIAN_FRONTEND=noninteractive apt-get install -y ansible git"
  end
end
