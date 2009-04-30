typedef struct _lua_drv_t {
  ErlDrvPort port;
  lua_State *L;
} lua_drv_t;


#ifdef __cplusplus
extern "C" {
#endif

  /* Fix a silly Lua warning */
  
  void luaL_openlibs (lua_State *L);

  /* Commands */

  void erl_lua_call(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_concat(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_getfield (lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_getglobal(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_gettop(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_pushboolean(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_pushinteger(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_pushstring(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_pushnil(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_pushnumber(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_remove(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_setfield(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_setglobal(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_toboolean(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_tointeger(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_tolstring(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_tonumber(lua_drv_t *driver_data, char *buf, int index);
  void erl_lua_type(lua_drv_t *driver_data, char *buf, int index);
  
  void erl_lual_dostring (lua_drv_t *driver_data, char *buf, int index);
  
  void erl_lua_no_command (lua_drv_t *driver_data);

#ifdef __cplusplus
}
#endif				/* __cplusplus */
