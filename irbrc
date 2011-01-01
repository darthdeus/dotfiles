require 'rubygems'

unless defined?(Rails) || ENV.include?('RAILS_ENV')
  require 'irb/completion'
  require 'map_by_method'
  require 'what_methods'
  require 'ap'
  require 'pp'
  require 'wirble'

  IRB.conf[:AUTO_INDENT] = true
  IRB.conf[:PROMPT_MODE] = :SIMPLE
  IRB.conf[:SAVE_HISTORY] = 100
  #IRB.conf[:HISTORY_FALE] = "#{ENV['HOME']}/.irb_history"

  Wirble.init(:skip_prompt => true, :skip_history => true)
  Wirble.colorize


  def clear
    print "\e[He[2j\e[2J"
  end
end
