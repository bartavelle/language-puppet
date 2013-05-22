require 'erb'
require 'digest/md5'

class Scope
    def initialize(context,variables)
        @context = context
        @variables = variables
    end

    def lookupvar(name)
        x = varlookup(@context,@variables,name)
        if x == :undef
            throw("Unknown variable " + name + " error: " + x.to_s)
        else
            x
        end
    end

    def has_variable?(name)
        x = varlookup(@context,@variables,name)
        if x == :undef
            false
        else
            true
        end
    end
end

class ErbBinding
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
    def self.runFromFile(filename,context,variables)
        self.runFromContent(IO.read(filename),context,variables)
    end
    def self.runFromContent(content,context,variables)
        nerb = ERB.new(content, nil, "-")
        binding = ErbBinding.new(context,variables).get_binding
        nerb.result(binding)
    end
end

