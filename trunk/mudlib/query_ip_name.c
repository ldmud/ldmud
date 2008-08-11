/* IPv4/IPv6 name lookup using ERQ
 * by Fini <ujastrow@mail.desy.de>
 *    Tue, 9 Mar 2004 08:20:16 +0100 (MET)
 */

void get_ip_name(string ip_num) {
  [...]
      num=allocate(4);
    if (sscanf(ip_num, "%d.%d.%d.%d", num[0], num[1], num[2], num[3])!=4)
      continue;
#   ifdef RDNS_DEBUG
      log_file("IP-NAME",
        sprintf(" ** RDNS request for IPv4 %s sent\n", ip_num));
#   endif
    send_erq(ERQ_RLOOKUP, num, #'_got_ipv4_name);
  [...]
}

void _got_ipv4_name(int* data, int len) {
  int* x;
  string ip_num, ip_nam;
  if (previous_object()) return; // only the driver may call this
  if (!pointerp(data) || len<5) return;

  x=allocate(4);
  if ((x[0]=data[0])<0) x[0]+=256; // not needed with newer drivers
  if ((x[1]=data[1])<0) x[1]+=256;
  if ((x[2]=data[2])<0) x[2]+=256;
  if ((x[3]=data[3])<0) x[3]+=256;
  ip_num=sprintf("%d.%d.%d.%d", x[0], x[1], x[2], x[3]);
  ip_nam=to_string(data[4..]);
# ifdef RDNS_DEBUG
    log_file("IP-NAME", sprintf("got v4: %s -> %s\n", ip_num, ip_nam));
# endif

  if (ip_num==ip_nam) return; // not resolved
  if (!ip_map) ip_map=get_extra_wizinfo(0)[IP_NAMES];
  ip_map[ip_num, 0]=ip_nam;
  ip_map[ip_num, 1]=time();
}
