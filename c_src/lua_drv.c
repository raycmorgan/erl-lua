#include <erl_driver.h>
#include <ei.h>
#include <lua.h>
#include <lauxlib.h>
#include <string.h>
#include <stdio.h>

#include "lua_drv.h"
#include "commands.h"

static ErlDrvData start (ErlDrvPort port, char* cmd);
static void stop (ErlDrvData handle);
static void process (ErlDrvData handle, ErlIOVec *ev);

static ErlDrvEntry lua_driver_entry = {
    NULL,                             /* init */
    start,                            /* startup */
    stop,                             /* shutdown */
    NULL,                             /* output */
    NULL,                             /* ready_input */
    NULL,                             /* ready_output */
    "lua_drv",                        /* the name of the driver */
    NULL,                             /* finish */
    NULL,                             /* handle */
    NULL,                             /* control */
    NULL,                             /* timeout */
    process,                          /* process */
    NULL,                             /* ready_async */
    NULL,                             /* flush */
    NULL,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING     /* ERL_DRV_FLAGs */
};

DRIVER_INIT(lua_driver) {
  return &lua_driver_entry;
}

static ErlDrvData
start(ErlDrvPort port, char *cmd)
{
  lua_State *L;
  L = luaL_newstate();
  luaL_openlibs(L);
  
  lua_drv_t* retval = (lua_drv_t*) driver_alloc(sizeof(lua_drv_t));
  retval->port = port;
  retval->L = L;
  
  return (ErlDrvData) retval;
}

static void
stop(ErlDrvData handle)
{
  lua_drv_t* driver_data = (lua_drv_t*) handle;
  lua_close(driver_data->L);
  driver_free(driver_data);
}

static void
process(ErlDrvData handle, ErlIOVec *ev)
{
  lua_drv_t *driver_data = (lua_drv_t*) handle;
  char *buf = ev->binv[1]->orig_bytes;
  int index = 0;
  int arty, version;
  long command;
  
  ei_decode_version(buf, &index, &version);
  ei_decode_tuple_header(buf, &index, &arty);
  ei_decode_long(buf, &index, &command);
  
  // printf("Command: %ld\n", command);
  // printf("sizeof: int: %ld, long: %ld, long long: %ld\n", sizeof(int), sizeof(long), sizeof(long long));
  
  switch(command) {
  case ERL_LUA_CALL:
    erl_lua_call(driver_data, buf, index);
    break;
  case ERL_LUA_CONCAT:
    erl_lua_concat(driver_data, buf, index);
    break;
  case ERL_LUA_GETFIELD:
    erl_lua_getfield(driver_data, buf, index);
    break;
  case ERL_LUA_GETGLOBAL:
    erl_lua_getglobal(driver_data, buf, index);
    break;
  case ERL_LUA_GETTOP:
    erl_lua_gettop(driver_data, buf, index);
    break;
  case ERL_LUA_PUSHBOOLEAN:
    erl_lua_pushboolean(driver_data, buf, index);
    break;
  case ERL_LUA_PUSHINTEGER:
    erl_lua_pushinteger(driver_data, buf, index);
    break;
  case ERL_LUA_PUSHSTRING:
    erl_lua_pushstring(driver_data, buf, index);
    break;
  case ERL_LUA_PUSHNIL:
    erl_lua_pushnil(driver_data, buf, index);
    break;
  case ERL_LUA_PUSHNUMBER:
    erl_lua_pushnumber(driver_data, buf, index);
    break;
  case ERL_LUA_REMOVE:
    erl_lua_remove(driver_data, buf, index);
    break;
  case ERL_LUA_SETFIELD:
    erl_lua_setfield(driver_data, buf, index);
    break;
  case ERL_LUA_SETGLOBAL:
    erl_lua_setglobal(driver_data, buf, index);
    break;
  case ERL_LUA_TOBOOLEAN:
    erl_lua_toboolean(driver_data, buf, index);
    break;
  case ERL_LUA_TOINTEGER:
    erl_lua_tointeger(driver_data, buf, index);
    break;
  case ERL_LUA_TOLSTRING:
    erl_lua_tolstring(driver_data, buf, index);
    break;
  case ERL_LUA_TONUMBER:
    erl_lua_tonumber(driver_data, buf, index);
    break;
  case ERL_LUA_TYPE:
    erl_lua_type(driver_data, buf, index);
    break;
  
  case ERL_LUAL_DOSTRING:
    erl_lual_dostring(driver_data, buf, index);
    break;
  
  default:
    erl_lua_no_command(driver_data);
  }
}
