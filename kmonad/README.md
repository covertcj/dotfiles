# Ubuntu Install

Use the file in the config directory as an example service.

```
1. Adjust the target `.kbd` file
2. Copy it somewhere like /etc/systemd/system/<machine>_<device>.service
3. sudo systemctl daemon-reload
4. sudo systemctl enable <service_name>
5. sudo systemctl start <service_name>
6. sudo systemctl status <service_name>
```

The config directory also contains an example udev rule. To find the value you need for the KERNEL field, you can use a command like the following, updating it to point toward your USB device.
```
udevadm info --attribute-walk --path=$(udevadm info --query=path --name=/dev/input/by-path/pci-0000:00:14.0-usb-0:4.3:1.2-event-kbd)
```
