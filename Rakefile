task :default do
  sh "(cd c_src; rake compile)"
  print "Compiling Erlang sources..."
  sh "erlc -Iinclude/ -o ebin/ src/*.erl"
  print " done\n"
end

task :clean do
  print "Cleaning..."
  sh "rm ebin/*.beam"
  sh "rm priv/*.so"
  print " done\n"
end

task :run do
  sh "erl -pa ebin"
end