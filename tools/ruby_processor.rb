
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

  def process node
    if node
      node.location.expression.tap do |loc|
        next if loc.nil?
        @spans << [ node.type, loc.begin_pos + 1, loc.end_pos + 1 ]
      end
    end
    super
  end
  
  private
  def add_span node, type = nil
    node.location.expression.tap do |loc|
      @spans << [ type || node.type, loc.begin_pos + 1, loc.end_pos + 1 ]
    end
  end
  
end
