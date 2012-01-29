require 'rubygems'

# unless defined?(Rails) || ENV.include?('RAILS_ENV')
#  require 'irb/completion'
#  require 'irb/ext/save-history'

#  require 'map_by_method'
#  require 'what_methods'
#  require 'ap'
#  require 'pp'
#  require 'wirble'

IRB.conf[:AUTO_INDENT] = true
IRB.conf[:PROMPT_MODE] = :SIMPLE
IRB.conf[:SAVE_HISTORY] = 1000
IRB.conf[:HISTORY_FALE] = "#{ENV['HOME']}/.irb_history"

#  Wirble.init(:skip_prompt => true, :skip_history => true)
#  Wirble.colorize


def clear
  print "\e[He[2j\e[2J"
end
# end

if defined?(Rails)
  def change_log(stream)
    ActiveRecord::Base.logger = Logger.new(stream)
    ActiveRecord::Base.clear_active_connections!
  end

  def show_log
    change_log(STDOUT)  
  end

  def hide_log
    change_log(nil)
  end
end
