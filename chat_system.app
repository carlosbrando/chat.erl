{application, chat_system,
  [{description, "Chat System"},
  {vsn, "1.0"},
  {modules, [chat_client, message_router, misc_server,
	           message_router_sup, message_store, server_util]},
  {registered, [message_router, message_store]},
  {applications, [kernel, stdlib, mnesia]},
  {env, []},
  {mod, {chat_system, []}}]}.
