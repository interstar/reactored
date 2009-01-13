require 'net/http'
require 'date'

#REMOTE_URI = "http://alan-woods-macbook.local:8000/reactor/_/tag/founder"
REMOTE_URI = "http://alan-woods-macbook.local:8000/reactor/_/echo/test"
TIMEOUT = 10
TOKEN = "security-token"
Microecs_per_day = 24 * 60 * 60 * 1000000
IDENTITIES = "_/id/"
ACL = "_/acl/"

class Reactor
  def initialize(host,context,token)
    @host = host
    @context = context
    @token = token
    @baseurl = host + "/" + context +"/"
    end

  def create(resource,title,description,*attributes)
    basic = { :title => title,:description => description}
    attribs = attributes.any? ?  attributes[0].merge(basic) : basic
    url = url(resource)
    post(url,attribs)
  end

  def clear(domain)
    url = url(domain + "/")
    delete(url)
  end

  def new_user(title,id,nickname,password)
    attribs = {:title => title, :email => id, :nick => nickname, :password => password}
    url = url(IDENTITIES)
    post(url,attribs)
  end

  def grant(id,acl,resource)
    update_acl("grant",id,acl,resource)
  end

  def revoke(id,acl,resource)
    update_acl("revoke",id,acl,resource)
  end

  def update_acl(control,id,acl,resource)
    attribs = {:control => control, :acl => acl.join(","), :resource => resource}
    url = url(ACL + id )
    post(url,attribs)
  end

  def react(resource,request)
    begin
      Timeout::timeout(TIMEOUT) do 
        res = Net::HTTP.new(resource.host, resource.port).start do |http| 
          http.request(request) 
        end
      end
    rescue Timeout::Error 
      puts "http request timed out, after #{TIMEOUT} seconds" 
    end 
  end

  def delete(url)
    request = Net::HTTP::Delete.new(url.path,{TOKEN,@token}) 
    react(url,request)
  end

  def post(url,attribs)
    request = Net::HTTP::Post.new(url.path,{TOKEN,@token}) 
    request.set_form_data(attribs) 
    react(url,request)
  end

  def url(path)
    url = URI.parse(@baseurl + path)
    url.path = "/" if url.path.length < 1
    url
  end

  def date_to_int(date)
    ms = 0
    parts = date.split("-")
    if parts.length ==3 then
      day = parts[0].to_i
      month = parts[1].to_i
      year = parts[2].to_i
      d = Date.new(year,month,day)
      e = Date.new(1970,1,1)
      days = d - e
      ms = Microecs_per_day * days.to_i
    end
    return ms
  end

end

#reactor = Reactor.new("http://alan-woods-macbook.local:8000","reactor","1215002875465882")
#puts reactor.create("_/echo/test","Test","Tester in action")
#puts reactor.create("_/echo/test","Test","Tester in action",{:test => "test"})

#puts reactor.create("news/","Test Run 1","Test create news item 1st run")



