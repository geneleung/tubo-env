handle SIGUSR1 nostop
handle SIGUSR2 nostop
set breakpoint pending on

define bta
  thread apply all bt
end
