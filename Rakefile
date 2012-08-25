
$quiet = false
desc "Silence all console output"
task :quiet do
  $quiet = true
end


desc "Clean out all temp files"
task :clean do
  clean_files = %w{
    *.c
    *.o
    *.so
    *.import.scm
    salmonella.log
  }

  Dir[*clean_files].each do |file|
    puts "Deleting #{file}..." unless $quiet
    File.delete(file)
  end
end


desc "Run the automated test suite"
task :spec do
  if $quiet
    `csi -s tests/run.scm`
  else
    system "csi -s tests/run.scm"
  end
  exit $?.exitstatus
end

desc "Same as rake spec"
task :test => [:spec]

task :default => [:spec]
