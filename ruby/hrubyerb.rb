require 'erb'
require 'digest/md5'

class Scope
    def initialize(context,variables)
        @context = context
        @variables = variables
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

    def get_hash
        vl('~g~e~t_h~a~s~h~')
    end
end

class ErbBinding
    @options = {}
    def initialize(context,variables)
        @scope = Scope.new(context,variables)
    end
    def get_binding
        return binding()
    end
    def has_variable?(name)
        @scope.has_variable?(name.to_s)
    end
    def method_missing(sname)
        name = sname.to_s
        if name == 'scope'
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
        # binding = ErbBinding.new(context,variables).get_binding
        nerb.result(binding.get_binding)
    end
end

