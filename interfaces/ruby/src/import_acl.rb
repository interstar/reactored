require 'rexml/document'
require 'reactor_util.rb'
include REXML

STOKEN = "1216079807158937" 
URL = "http://127.0.0.1:8080"
#STOKEN = "1216080767569823" 
#URL = "http://awmb.local:8080"
PATH = "./"

def add_user(entry,reactor,domain)
  id = entry.attributes["id"]
  nick = entry.attributes["nick"]
  title = entry.attributes["title"]
  password = entry.attributes["password"]
  puts title  + " id:" + id + ",nick:" + nick
  #reactor.new_user(title,id,nick,password) 
end

def add_acl(entry,reactor,domain)
  type = entry.attributes["type"]
  user = entry.attributes["user"]
  item = entry.attributes["item"]
  privelages = entry.attributes["privelages"]
  puts type +  "  " + user+ " " + privelages + " on " + item
  #reactor.grant(user,privelages.split(","),domain + item)
end

reactor = Reactor.new(URL,"reactor",STOKEN)
doc = Document.new(File.new(PATH + "acl.xml"))
root = doc.root
domain = root.attributes["domain"]
puts "Adding users : \n" 
temp = root.each_element('//user') {|entry| add_user(entry,reactor,domain)}
puts "updating acls : \n" 
temp = root.each_element('//acl') {|entry| add_acl(entry,reactor,domain)}

