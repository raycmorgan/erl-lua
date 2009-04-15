task :default => [:make_c, :make_erl]

task :test => [:default, :make_tests] do
  sh "erl -pa ebin -noshell -s lua_test test -s init stop"
end

task :clean do
  print "Cleaning..."
  sh "rm ebin/*.beam"
  sh "rm priv/*.so"
  print " done\n"
end

task :make_c do
  sh "(cd c_src; rake compile)"
end

task :make_erl do
  print "Compiling Erlang sources..."
  sh "erlc -Iinclude/ -o ebin/ src/*.erl"
  print " done\n"
end

task :make_tests => :default do
  print "Compiling Erlang test sources..."
  sh "erlc -Iinclude/ -o ebin/ tests/*.erl"
  print " done\n"
end

task :run do
  sh "erl -pa ebin"
end