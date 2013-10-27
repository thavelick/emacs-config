require "awesome_print"
require "JSON"
require "pp"

script = __FILE__
mode = ARGV[0]

if ARGV.length < 1
  puts "Usage cat [filename] #{script} [--to-json|--from-json]"
  exit(1)
end

input = STDIN.read

if mode == "--to-json"
  print JSON.pretty_generate(eval(input))
elsif mode == "--from-json"
  ap JSON.parse(input), {
    :indent => 2,
    :plain => true,
    :index => false
  }
end
