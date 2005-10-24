
/* string grg_enc(string, void|string)
 * string grg_dec(string, void|string) 
 * 
 * First argument for grg_enc (grg_dec) is the plaintext (ciphertext).
 * If the second argument is a string, this string is directly used as
 * the passphrase. If there is no second argument supplied, a random
 * password is generated and stored in a database-file associated with
 * the load_name of the object (if there already exists a password for
 * the current object, the old one is used.).
 * It is possible to extend the third arguments to accept type object,
 * which, however, will be a security reason. In that case, the supplied
 * object is used instead of the current_object.
 */

#include "config.h"

#include "driver.h"
#include "mstrings.h"
#include "object.h"
#include "simulate.h"

#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <db_185.h>
#include <libgringotts.h>

#define KEYFILE "/tmp/ff_kstore.db"

#define DEFAULTALGO GRG_AES

DB *keystore = NULL;

static void open_kst(void) {
   if (keystore)
     return;
   keystore = dbopen(KEYFILE, O_RDWR|O_CREAT, 0600, DB_HASH, NULL);
   if (!keystore)
     error("Error opening keystore: %s\n", strerror(errno));
}
   

static char *_getkey (char *id) {
   DBT name, data;
   int st;
   
   open_kst();
   
   name.data = id;
   name.size = strlen(id)+1;

   st = keystore->get(keystore, &name, &data, 0);
   if (st < 0) {
      keystore->close(keystore);
      keystore = NULL;
      error("Error retrieving id '%s': %s\n", id, strerror(errno));
   } else if (st > 0)
     return NULL;
   
   return data.data;
}

static void _setkey (char *id, char *key) {
   DBT name, data;
   int st;
   
   open_kst();
   
   name.data = id;
   name.size = strlen(id)+1;
   data.data = key;
   data.size = strlen(key)+1;
   
   st = keystore->put(keystore, &name, &data, 0);
   if (st < 0) {
      keystore->close(keystore);
      keystore = NULL;
      error("Error inserting id '%s': %s\n", id, strerror(errno));
   }
   keystore->sync(keystore, 0);
}

static char *_genpwd (void) {
   static char pwd[17];
   static int initialized = 0;
   int i;
   
   // This function should be improved to provide more secure passwords...
   
   if (!initialized) {
      srandom(time(NULL)+getpid()); // The stack-address of i should be hard to guess...
      initialized = 1;
   }
   
   for (i = 0; i < 16; i++) {
      pwd[i] = random()%90+33;
   }
   pwd[16] = '\0';
   
   return pwd;
}

static char *_encrypt(void *data, long *len, char *key, int algo) {
   void *cryptdata;
   GRG_CTX ctx;
   GRG_KEY gkey;
   int st;
   
   ctx = grg_context_initialize_defaults("SOD");
   if (!ctx)
     error("Internal error: Could not init grg-context\n");
   gkey = grg_key_gen(key, -1);
   if (!gkey) {
      grg_context_free(ctx);
      error("Internal error: Could not init grg-keystruct\n");
   }
   
   grg_ctx_set_crypt_algo (ctx, algo);
   
   st = grg_encrypt_mem(ctx, gkey, &cryptdata, len, data, *len);
   
   grg_key_free(ctx, gkey);
   grg_context_free(ctx);
   
   if (st)
     error("Encryption failed: %d (Out of memory?)\n", st);   
   
   return cryptdata;
}

static char *_decrypt(char *cryptdata, long *len, char *key) {
   unsigned char *data;
   GRG_CTX ctx;
   GRG_KEY gkey;
   int st;
   
   ctx = grg_context_initialize_defaults("SOD");
   if (!ctx)
     error("Internal error: Could not init grg-context\n");
   gkey = grg_key_gen(key, -1);
   if (!gkey) {
      grg_context_free(ctx);
      error("Internal error: Could not init grg-keystruct\n");
   }
   
   st = grg_decrypt_mem(ctx, gkey, cryptdata, *len, &data, len);
   
   grg_key_free(ctx, gkey);
   grg_context_free(ctx);
   
   if (st) {
      switch (st) {
       case GRG_READ_MAGIC_ERR:
	 return NULL;
       default:
	 error("Decryption failed: %d (wrong object/password?)\n",
	       st);
      }
   }
   
   return data;
}

svalue_t *v_grg_enc (svalue_t *sp, int argc) {
   char *id = NULL, *pwd = NULL, *data;
   int algo = -1;
   long len;
   
   if (argc == 3) {
      algo = sp->u.number;
      sp--;
      argc--;
   }
   
   if (argc == 2) {
      switch (sp->type) {
       case T_OBJECT:
	 id = sp->u.ob->load_name->str->txt;
	 break;
       case T_STRING:
	 pwd = sp->u.str->str->txt;
	 break;
       case T_NUMBER:
	 if (algo >= 0)
	   error("Wrong type of argument 2 (string or object).\n");
	 else
	   algo = sp->u.number;
	 id = current_object->load_name->str->txt;
      }
      sp--;
   } else
     id = current_object->load_name->str->txt;
   
   if (algo < 0)
     algo = DEFAULTALGO;
   
   if ((algo > 0x70) || (algo < 0x00) || (algo & 0x0F))
     error("Wrong algorithm.\n");
   
   if (!pwd) {
      pwd = _getkey(id);
      if (!pwd) {
	 pwd = _genpwd();
	 _setkey(id, pwd);
      }
   }
   data = sp->u.str->str->txt;
   len = sp->u.str->str->size;
   data = _encrypt(data, &len, pwd, algo);
   if (argc == 2)
     free_svalue(sp+1);
   free_svalue(sp);
   put_string(sp, new_n_mstring(data, len));
   free(data);
   
   return sp;
}

svalue_t *v_grg_dec (svalue_t *sp, int argc) {
   char *id = NULL, *pwd = NULL, *data;
   long len;
   
   if (argc == 2) {
      switch (sp->type) {
       case T_OBJECT:
	 id = sp->u.ob->load_name->str->txt;
	 break;
       case T_STRING:
	 pwd = sp->u.str->str->txt;
	 break;
      }
      sp--;
   } else
     id = current_object->load_name->str->txt;
   
   
   if (!pwd) {
      pwd = _getkey(id);
      if (!pwd)
	error("Could not retrieve credentials to decrypt data for '%s'.\n",
	      id);
   }
   data = sp->u.str->str->txt;
   len = sp->u.str->str->size;
   data = _decrypt(data, &len, pwd);
   if (argc == 2)
     free_svalue(sp+1);
   free_svalue(sp);
   if (data) {
      put_string(sp, new_n_mstring(data, len));
      free(data);
   } else
     put_number(sp, 0);
   
   return sp;
}
