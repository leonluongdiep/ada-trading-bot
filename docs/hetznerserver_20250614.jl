hetznerserver_20250614
PS C:\freeding\tbot052025> ssh trading@91.99.11.170
Welcome to Ubuntu 24.04.2 LTS (GNU/Linux 6.8.0-60-generic x86_64)

 * Documentation:  https://help.ubuntu.com
 * Management:     https://landscape.canonical.com
 * Support:        https://ubuntu.com/pro

 System information as of Sat Jun 14 09:37:35 PM UTC 2025

  System load:  0.0                Processes:             123
  Usage of /:   11.4% of 37.23GB   Users logged in:       1
  Memory usage: 9%                 IPv4 address for eth0: 91.99.11.170
  Swap usage:   0%                 IPv6 address for eth0: 2a01:4f8:c013:d2e1::1

 * Strictly confined Kubernetes makes edge and IoT secure. Learn how MicroK8s  
   just raised the bar for easy, resilient and secure K8s cluster deployment.  

   https://ubuntu.com/engage/secure-kubernetes-at-the-edge

Expanded Security Maintenance for Applications is not enabled.

0 updates can be applied immediately.

Enable ESM Apps to receive additional future security updates.
See https://ubuntu.com/esm or run: sudo pro status


Last login: Sat Jun 14 21:18:40 2025 from 95.91.227.227
trading@freedingserver1:~$ git status
fatal: not a git repository (or any of the parent directories): .git
trading@freedingserver1:~$ git remote -v
fatal: not a git repository (or any of the parent directories): .git
trading@freedingserver1:~$ ls -la ~/ada-trading/
total 32
drwxrwxr-x  8 trading trading 4096 Jun  9 22:22 .
drwxr-x--- 15 trading trading 4096 Jun 14 21:30 ..
drwxrwxr-x  2 trading trading 4096 Jun  9 22:22 backups
drwxrwxr-x  2 trading trading 4096 Jun  9 22:42 configs
drwxrwxr-x  2 trading trading 4096 Jun  9 22:22 logs
drwxrwxr-x  5 trading trading 4096 Jun  9 22:42 python_bot
drwxrwxr-x  5 trading trading 4096 Jun  9 22:22 r_analysis
drwxrwxr-x  5 trading trading 4096 Jun  9 22:22 shared_data
trading@freedingserver1:~$ git status
fatal: not a git repository (or any of the parent directories): .git
trading@freedingserver1:~$ git remote -v
fatal: not a git repository (or any of the parent directories): .git
trading@freedingserver1:~$ ls -la ~/ada-trading/venv/
ls: cannot access '/home/trading/ada-trading/venv/': No such file or directory
trading@freedingserver1:~$ source ~/ada-trading/venv/bin/activate
-bash: /home/trading/ada-trading/venv/bin/activate: No such file or directory
trading@freedingserver1:~$ pip list
Package               Version
--------------------- --------------
attrs                 23.2.0
Automat               22.10.0
Babel                 2.10.3
bcrypt                3.2.2
blinker               1.7.0
boto3                 1.34.46
botocore              1.34.46
certifi               2023.11.17
chardet               5.2.0
click                 8.1.6
cloud-init            25.1.2
colorama              0.4.6
command-not-found     0.3
configobj             5.0.8
constantly            23.10.4
cryptography          41.0.7
dbus-python           1.3.2
distro                1.9.0
distro-info           1.7+build1
httplib2              0.20.4
hyperlink             21.0.0
idna                  3.6
incremental           22.10.0
Jinja2                3.1.2
jmespath              1.0.1
jsonpatch             1.32
jsonpointer           2.0
jsonschema            4.10.3
launchpadlib          1.11.0
lazr.restfulclient    0.14.6
lazr.uri              1.0.6
markdown-it-py        3.0.0
MarkupSafe            2.1.5
mdurl                 0.1.2
netifaces             0.11.0
oauthlib              3.2.2
packaging             24.0
pexpect               4.9.0
pip                   24.0
ptyprocess            0.7.0
pyasn1                0.4.8
pyasn1-modules        0.2.8
Pygments              2.17.2
PyGObject             3.48.2
PyHamcrest            2.1.0
PyJWT                 2.7.0
pyOpenSSL             23.2.0
pyparsing             3.1.1
pyrsistent            0.20.0
pyserial              3.5
python-apt            2.7.7+ubuntu4
python-dateutil       2.8.2
python-debian         0.1.49+ubuntu2
python-magic          0.4.27
pytz                  2024.1
PyYAML                6.0.1
requests              2.31.0
rich                  13.7.1
s3transfer            0.10.1
service-identity      24.1.0
setuptools            68.1.2
six                   1.16.0
sos                   4.8.2
ssh-import-id         5.11
systemd-python        235
Twisted               24.3.0
ubuntu-drivers-common 0.0.0
ubuntu-pro-client     8001
ufw                   0.36.2
unattended-upgrades   0.1
urllib3               2.0.7
wadllib               1.3.6
wheel                 0.42.0
xkit                  0.0.0
zope.interface        6.1
trading@freedingserver1:~$ # Sind die Trading Services installiert?
ist-unittrading@freedingserver1:~$ systemctl list-unit-files | grep ada
bot
systemctl status ada-monitoringtrading@freedingserver1:~$ systemctl status ada-trading-bot
Unit ada-trading-bot.service could not be found.
trading@freedingserver1:~$ systemctl status ada-monitoring
Unit ada-monitoring.service could not be found.
trading@freedingserver1:~$ # Repository Check
trading@freedingserver1:~$ ls -la ~/
 2>/dtotal 88
drwxr-x--- 15 trading trading  4096 Jun 14 21:38 .
drwxr-xr-x  3 root    root     4096 Jun  9 19:34 ..
drwxrwxr-x  8 trading trading  4096 Jun  9 22:22 ada-trading
drwxrwxr-x  2 trading trading  4096 Jun  9 21:57 backups
-rw-------  1 trading trading 11897 Jun 11 21:21 .bash_history
-rw-r--r--  1 trading trading   220 Jun  9 19:34 .bash_logout
-rw-r--r--  1 trading trading  3897 Jun  9 22:26 .bashrc
drwx------  6 trading trading  4096 Jun 14 21:23 .cache
-rw-r--r--  1 trading trading     0 Jun  9 19:34 .cloud-locale-test.skip
drwx------  3 trading trading  4096 Jun 14 21:27 .config
drwxrwxr-x  2 trading trading  4096 Jun  9 21:57 configs
-rw-------  1 trading trading    77 Jun 14 21:38 .lesshst
drwxrwxr-x  3 trading trading  4096 Jun  9 22:29 .local
drwxrwxr-x  2 trading trading  4096 Jun  9 21:57 logs
-rw-r--r--  1 trading trading   807 Jun  9 19:34 .profile
drwxrwxr-x  5 trading trading  4096 Jun  9 21:57 python_bot
drwxrwxr-x  3 trading trading  4096 Jun  9 20:32 R
drwxrwxr-x  5 trading trading  4096 Jun  9 21:57 r_analysis
drwxrwxr-x  5 trading trading  4096 Jun  9 21:57 shared_data
drwx------  2 trading trading  4096 Jun  9 19:43 .ssh
-rw-r--r--  1 trading trading     0 Jun  9 19:50 .sudo_as_admin_successful
drwxrwxr-x  5 trading trading  4096 Jun  9 22:21 venv
ev/null || echo "Repository nicht gefunden"

#trading@freedingserver1:~$ ls -la ~/ada-trading/ 2>/dev/null || echo "Repository nicht gefunden"
total 32
drwxrwxr-x  8 trading trading 4096 Jun  9 22:22 .
drwxr-x--- 15 trading trading 4096 Jun 14 21:38 ..
drwxrwxr-x  2 trading trading 4096 Jun  9 22:22 backups
drwxrwxr-x  2 trading trading 4096 Jun  9 22:42 configs
drwxrwxr-x  2 trading trading 4096 Jun  9 22:22 logs
drwxrwxr-x  5 trading trading 4096 Jun  9 22:42 python_bot
drwxrwxr-x  5 trading trading 4096 Jun  9 22:22 r_analysis
drwxrwxr-x  5 trading trading 4096 Jun  9 22:22 shared_data
trading@freedingserver1:~$
trading@freedingserver1:~$ # Services Check  
trading@freedingserver1:~$ systemctl --user list-unit-files | grep ada
| grep adatrading@freedingserver1:~$ sudo systemctl list-unit-files | grep ada
[sudo] password for trading: 
trading@freedingserver1:~$ # Repository Check
trading@freedingserver1:~$ ls -la ~/
total 88
drwxr-x--- 15 trading trading  4096 Jun 14 21:38 .
drwxr-xr-x  3 root    root     4096 Jun  9 19:34 ..
drwxrwxr-x  8 trading trading  4096 Jun  9 22:22 ada-trading
drwxrwxr-x  2 trading trading  4096 Jun  9 21:57 backups
-rw-------  1 trading trading 11897 Jun 11 21:21 .bash_history
-rw-r--r--  1 trading trading   220 Jun  9 19:34 .bash_logout
-rw-r--r--  1 trading trading  3897 Jun  9 22:26 .bashrc
drwx------  6 trading trading  4096 Jun 14 21:23 .cache
-rw-r--r--  1 trading trading     0 Jun  9 19:34 .cloud-locale-test.skip
drwx------  3 trading trading  4096 Jun 14 21:27 .config
drwxrwxr-x  2 trading trading  4096 Jun  9 21:57 configs
-rw-------  1 trading trading    77 Jun 14 21:38 .lesshst
drwxrwxr-x  3 trading trading  4096 Jun  9 22:29 .local
drwxrwxr-x  2 trading trading  4096 Jun  9 21:57 logs
-rw-r--r--  1 trading trading   807 Jun  9 19:34 .profile
drwxrwxr-x  5 trading trading  4096 Jun  9 21:57 python_bot
drwxrwxr-x  3 trading trading  4096 Jun  9 20:32 R
drwxrwxr-x  5 trading trading  4096 Jun  9 21:57 r_analysis
drwxrwxr-x  5 trading trading  4096 Jun  9 21:57 shared_data
drwx------  2 trading trading  4096 Jun  9 19:43 .ssh
-rw-r--r--  1 trading trading     0 Jun  9 19:50 .sudo_as_admin_successful
drwxrwxr-x  5 trading trading  4096 Jun  9 22:21 venv
trading@freedingserver1:~$ ls -la ~/ada-trading/ 2>/dev/null || echo "Repository nicht gefunden"
t-files | grep ada
sudo systemctl list-unit-files total 32
drwxrwxr-x  8 trading trading 4096 Jun  9 22:22 .
drwxr-x--- 15 trading trading 4096 Jun 14 21:38 ..
drwxrwxr-x  2 trading trading 4096 Jun  9 22:22 backups
drwxrwxr-x  2 trading trading 4096 Jun  9 22:42 configs
drwxrwxr-x  2 trading trading 4096 Jun  9 22:22 logs
drwxrwxr-x  5 trading trading 4096 Jun  9 22:42 python_bot
drwxrwxr-x  5 trading trading 4096 Jun  9 22:22 r_analysis
drwxrwxr-x  5 trading trading 4096 Jun  9 22:22 shared_data
trading@freedingserver1:~$
trading@freedingserver1:~$ # Services Check
trading@freedingserver1:~$ systemctl --user list-unit-files | grep ada
| grep adatrading@freedingserver1:~$ sudo systemctl list-unit-files | grep ada
trading@freedingserver1:~$ 