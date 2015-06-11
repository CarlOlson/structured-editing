
require 'parser/current'

class RubyProcessor < Parser::AST::Processor

  attr_reader :spans
  
  def initialize *args
    super
    @spans = []
  end
  
  def self.parse source
    ast = Parser::CurrentRuby.parse source
    processor = self.new
    processor.process ast
    processor
  end

  def on_class node
    add_span node
    super
  end
  
  def on_def node
    add_span node
    super
  end

  def on_defs node
    add_span node
    super
  end

  def on_if node
    add_span node
    super
  end

  def on_while node
    add_span node
    super
  end
  
  private
  def add_span node
    node.location.expression.tap do |loc|
      @spans << [ node.type, loc.begin_pos, loc.end_pos ]
    end
  end
  
end
