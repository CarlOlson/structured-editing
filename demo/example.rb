class ExampleClass

  def initialize
    puts "ExampleClass"
  end

  def foobar
    if true
      puts "from foobar"
    else
      raise StandardError
    end
    
    while true
      break if true
    end
  end

end
