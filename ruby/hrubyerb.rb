require 'erb'
require 'digest/md5'
require 'yaml'

class Scope
    def initialize(context,variables,filename,stt,rdr)
        @context = context
        @variables = variables
        @file = filename
        @stt = stt
        @rdr = rdr
    end

    def [](key)
        lookupvar(key)
    end

    def vl(name)
        if name.start_with?("::")
            name = name[2..-1]
        end
        varlookup(@context,@variables,name)
    end

    def lookupvar(name)
        if name == "file"
            return @file
        end
        x = vl(name)
        if x == :undef
            throw("Unknown variable " + name)
        else
            x
        end
    end

    def has_variable?(name)
        x = vl(name)
        if x == :undef
            false
        else
            true
        end
    end

    def to_hash
        vl('~g~e~t_h~a~s~h~')
    end

    def function_to_yaml(args)
        args.to_yaml
    end

    def method_missing(sname,*args,&block)
        name = sname.to_s
        if name.start_with?('function_')
            fname = name[9..1000]
            o = callextfunc(fname, args, @stt, @rdr)
            case o
            when MyError
                throw o.getError()
            else
                return o
            end
        end
    end
end

class MyError
    def initialize(msg)
        @msg = msg
    end
    def getError
        @msg
    end
end

class ErbBinding
    @options = {}
    def initialize(context,variables,stt,rdr,filename='x')
        @stt = stt
        @rdr = rdr
        @scope = Scope.new(context,variables,filename,stt,rdr)
    end
    def get_binding
        return binding()
    end
    def has_variable?(name)
        @scope.has_variable?(name.to_s)
    end
    def method_missing(sname,*args,&block)
        name = sname.to_s
        if name.start_with?('function_')
            fname = name[9..1000]
            o = callextfunc(fname, args, @stt, @rdr)
            case o
            when MyError
                throw o.getError()
            else
                return o
            end
        elsif name == 'scope'
            @scope
        else
            @scope.lookupvar(name)
        end
    end
end

class Controller
    def self.runFromFile(filename,binding)
        self.runFromContent(IO.read(filename),binding)
    end
    def self.runFromContent(content,binding)
        nerb = ERB.new(content, nil, "-")
        nerb.result(binding.get_binding)
    end
end

