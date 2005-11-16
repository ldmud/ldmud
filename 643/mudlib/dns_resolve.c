void
dns_resolve (string hostname, closure callback)

// Try to lookup the IP for the fully qualified <hostname> using direct ERQ
// communication. Once the name is resolved, call callback() with the
// resolved name as parameter.
//
// Note: If the ERQ is unavailable, callback() is called with the original
// hostname.
//
// preferably to be installed as "simul efun." latest version available
// in world/net/library.i of psycMUVE (download from http://muve.pages.de)
// this version runs with broken and fixed erq's alike.
//
// Written by Tobias@Heldensaga and Arne@Elridion.

{
	closure c;

	if (sscanf(hostname,"%~D.%~D.%~D.%~D") == 4) {
	    // hostname is an IP already. Just call the callback.
	    funcall(callback, hostname);
	    return;
	}
	
       	c = lambda( ({ 'name }), ({ 
	(#'switch), ({ #'sizeof, 'name }),
	({ 4 }), // ERQ: name resolved!
	({ (#',),
		({ (#'=),'name,({ (#'map),'name,#'&,255 }) }),
		({ (#'=),'name,
		 ({
			(#'sprintf),"%d.%d.%d.%d",
						({ #'[, 'name, 0 }),
						({ #'[, 'name, 1 }),
						({ #'[, 'name, 2 }),
						({ #'[, 'name, 3 })
		  })
		 }),
		({ (#'funcall), callback, 'name })
	}),
	(#'break),
	({ 6 + strlen(hostname) }), // XERQ
	({ (#',),
                ({ (#'=),'name,({ (#'map),'name,#'&,255 }) }),
                ({ (#'=),'name,
                 ({
                        (#'sprintf),"%d.%d.%d.%d",
                                                ({ #'[, 'name, 1
}),
                                                ({ #'[, 'name, 2
}),
                                                ({ #'[, 'name, 3
}),
                                                ({ #'[, 'name, 4 })
                  })
                 }),
                ({ (#'funcall), callback, 'name })
        }),
	(#'break),
	({ #'default }),
	({
	 #'debug_message, "FATAL: ERQ could not resolve \""
		 + hostname + "\". The callback will not be called.\n"
		 }),
	(#'break)
	})
					);
#ifdef THEDAYWEARESURENOONERUNSABUGGYERQ
	if (!send_erq(ERQ_LOOKUP, hostname, c))
#else
	// appending the zero byte fixes a bug in erq. sick!
	if (!send_erq(ERQ_LOOKUP, to_array(hostname) + ({ 0 }), c))
#endif
	{
	    // if we cannot resolve using erq, we'll send back the hostname,
	    // so the driver can do a blocking resolve itself (needed for
	    // net_connect())
	    funcall(callback, hostname);
	}
}

#if 0

/*------------------------------------------------------------------------*/
// typical usage example taken from PSYCmuve world/net/irc/gatebot.c
// this ensures a non-blocking connect to a remote service even by name.
//
connect(host, port) {
	unless (host) host = IRCNET_HOST;
	unless (port) port = IRCNET_PORT;
	D2( D("IRCgate resolving hostname "+ host +"\n"); )
	dns_resolve(host, lambda(({ 'result }),
				 ({ #'connect2, 'result, port })));
}
connect2(host, port) {
#if __EFUN_DEFINED__(net_connect)
	int res;

	D0( D(S("IRCgate attempting connect to %O:%O\n", host, port)); )
	res = net_connect(host, port);
	if (res) monitor_report("_failure_network_connect_call",
		     "ircbot: connect to "+host+" returns "+ res);
	return res;
#else
	D2( D("IRCgate * Oh, driver does not provide net_connect()\n"); )
	return -9;
#endif
}

#endif /* EXAMPLE */

