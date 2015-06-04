
require 'parser/current'

class RubyProcessor < Parser::AST::Processor

  attr_reader :tokens
  
  def initialize *args
    super
    @tokens = []
  end
  
  def self.parse source
    ast = Parser::CurrentRuby.parse source
    processor = self.new
    processor.process ast
    processor
  end

  def on_class node
    add_token node
    super
  end
  
  def on_def node
    add_token node
    super
  end

  def on_defs node
    add_token node
  end

  private
  def add_token node
    node.location.expression.tap do |loc|
      @tokens << [ node.type, loc.begin_pos, loc.end_pos ]
    end
  end
  
end
