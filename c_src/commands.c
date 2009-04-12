#include <erl_driver.h>
#include <ei.h>
#include <lua.h>
#include <lauxlib.h>
#include <string.h>

#include "lua_drv.h"
#include "commands.h"

static void reply_ok(lua_drv_t *driver_data);
static void reply_error(lua_drv_t *driver_data);
static char* decode_string(char *buf, int *index);


void
erl_lua_call(lua_drv_t *driver_data, char *buf, int index)
{
  long args, results;
  
  ei_decode_long(buf, &index, &args);
  ei_decode_long(buf, &index, &results);
  
  lua_call(driver_data->L, args, results);
  
  reply_ok(driver_data);
}

void
erl_lua_concat(lua_drv_t *driver_data, char *buf, int index)
{
  long n;
  
  ei_decode_long(buf, &index, &n);
  
  lua_concat(driver_data->L, n);
  
  reply_ok(driver_data);
}

void
erl_lua_getfield(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  char *name;
  
  ei_decode_long(buf, &index, &i);
  name = decode_string(buf, &index);
  
  lua_getfield(driver_data->L, i, name);
  
  reply_ok(driver_data);
  free(name);
}

void
erl_lua_getglobal(lua_drv_t *driver_data, char *buf, int index)
{
  char *name;

  name = decode_string(buf, &index);
  
  lua_getglobal(driver_data->L, name);
  
  reply_ok(driver_data);
  free(name);
}

void
erl_lua_pushboolean(lua_drv_t *driver_data, char *buf, int index)
{
  int b;
  
  ei_decode_boolean(buf, &index, &b);
  
  lua_pushboolean(driver_data->L, b);
  
  reply_ok(driver_data);
}

void
erl_lua_pushinteger(lua_drv_t *driver_data, char *buf, int index)
{
  long num;
  
  ei_decode_long(buf, &index, &num);
  
  lua_pushinteger(driver_data->L, num);
  
  reply_ok(driver_data);
}

void
erl_lua_pushstring(lua_drv_t *driver_data, char *buf, int index)
{
  char *str;
  
  str = decode_string(buf, &index);
  
  lua_pushstring(driver_data->L, str);
  
  reply_ok(driver_data);
  free(str);
}

void
erl_lua_pushnil(lua_drv_t *driver_data, char *buf, int index)
{
  lua_pushnil(driver_data->L);
  reply_ok(driver_data);
}

void
erl_lua_pushnumber(lua_drv_t *driver_data, char *buf, int index)
{
  double num;
  
  ei_decode_double(buf, &index, &num);
  
  lua_pushnumber(driver_data->L, num);
  
  reply_ok(driver_data);
}

void
erl_lua_remove(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  
  ei_decode_long(buf, &index, &i);
  
  lua_remove(driver_data->L, i);
  
  reply_ok(driver_data);
}

void
erl_lua_setfield(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  char *name;
  
  ei_decode_long(buf, &index, &i);
  name = decode_string(buf, &index);
  
  lua_setfield(driver_data->L, i, name);
  
  reply_ok(driver_data);
  free(name);
}

void
erl_lua_setglobal(lua_drv_t *driver_data, char *buf, int index)
{
  char *name;
  
  name = decode_string(buf, &index);
  
  lua_setglobal(driver_data->L, name);
  
  reply_ok(driver_data);
  free(name);
}

void
erl_lua_toboolean(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  int res;
  ErlDrvTermData spec[2];
  spec[0] = ERL_DRV_ATOM;
  
  ei_decode_long(buf, &index, &i);
  
  res = lua_toboolean(driver_data->L, i);
  if (res)
    spec[1] = driver_mk_atom("true");
  else
    spec[1] = driver_mk_atom("false");
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

void
erl_lua_tointeger(lua_drv_t *driver_data, char *buf, int index)
{
  long i, res;
  
  ei_decode_long(buf, &index, &i);
  
  res = lua_tointeger(driver_data->L, i);
  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_INT, (ErlDrvTermData) res,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

void
erl_lua_tolstring(lua_drv_t *driver_data, char *buf, int index)
{
  unsigned long len;
  long i;
  const char *str;
  
  ei_decode_long(buf, &index, &i);
  
  str = lua_tolstring(driver_data->L, i, &len);
  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_STRING, (ErlDrvTermData) str, len,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

void
erl_lua_tonumber(lua_drv_t *driver_data, char *buf, int index)
{
  long i;
  double res;
  
  ei_decode_long(buf, &index, &i);
  
  res = lua_tonumber(driver_data->L, i);
  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_OK,
        ERL_DRV_FLOAT, (ErlDrvTermData) res,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}


void
erl_lual_dostring(lua_drv_t *driver_data, char *buf, int index)
{
  char *code;
  
  code = decode_string(buf, &index);
  
  if (!luaL_dostring(driver_data->L, code))
    reply_ok(driver_data);
  else
    reply_error(driver_data);
}


void
erl_lua_no_command(lua_drv_t *driver_data)
{  
  ErlDrvTermData spec[] = {
        ERL_DRV_ATOM,   ATOM_ERROR,
        ERL_DRV_STRING, (ErlDrvTermData) "No Command Found", 16,
        ERL_DRV_TUPLE,  2
  };
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}


static void
reply_ok(lua_drv_t *driver_data)
{
  ErlDrvTermData spec[] = {ERL_DRV_ATOM, ATOM_OK};
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}

static void
reply_error(lua_drv_t *driver_data)
{ 
  ErlDrvTermData spec[] = {ERL_DRV_ATOM, ATOM_ERROR};
  driver_output_term(driver_data->port, spec, sizeof(spec) / sizeof(spec[0]));
}


static char*
decode_string(char *buf, int *index)
{
  int type, length;
  char *str;
  
  ei_get_type(buf, index, &type, &length);
  str = malloc(sizeof(char) * (length + 1));
  ei_decode_string(buf, index, str);
  return str;
}