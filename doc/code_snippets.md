# Supervisor child spec
```erlang
child_spec()->
  % 定义用于启动子进程的函数调用。
  % 它必须是用作apply（M，F，A）的模块功能参数元组{M，F，A}。
  Start = {network_server, start_link, []},
  % 定义何时必须重新启动终止的子进程。
  % permanent: 子进程始终重新启动。
  % temporary: 子进程永远不会重新启动。
  % transient: 子进程仅在异常终止时（即，由于 normal，shutdown 或 {shutdown，Term} 以外的其他退出原因）异常终止时才重新启动。
  Restart = permanent,
  % Supervisor 通过调用exit（Child，shutdown）告诉子进程终止，然后等待退出信号，并从子进程中返回原因shutdown。
  % 如果在指定的毫秒数内未收到退出信号，则使用exit（Child，kill）无条件终止子进程。
  Shutdown = 20000,
  Type = worker,
  % 在代码替换期间，释放处理程序使用它来确定哪些进程正在使用某个模块。
  Modules = [network_server],
  %  ChildSpec = {network, Start, Restart, Shutdown, Type, Modules},
  ChildSpec = #{
    id => network_server,
    start => Start,
    restart => Restart,
    shutdown => Shutdown,
    type => Type,
    modules => Modules
  },
  % one_for_one: 如果一个子进程终止并要重新启动，则仅会影响该子进程。
  % one_for_all: 如果一个子进程终止并要重新启动，则所有其他子进程终止，然后重新启动所有子进程。
  % rest_for_one: 如果一个子进程终止并要重新启动，则子进程的“其余”（即，按照启动顺序在终止的子进程之后的子进程）将终止。
  % simple_one_for_one: 简化的 one_for_one，其中所有子进程都是动态添加的，具有相同进程类型（即，运行相同代码）的实例。
  RestartStrategy = one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 10,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  {ok, {SupFlags, [ChildSpec]}}.
```