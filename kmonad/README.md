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