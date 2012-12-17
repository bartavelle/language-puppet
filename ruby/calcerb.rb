require 'erb'
require 'digest/md5'

class Scope
    def initialize(scpv, ctx)
        @mvars = eval(scpv)
        @context = ctx
    end

    def lookupvar(name)
        if name.start_with?("::")
            name = name[2..-1]
        end
        if has_variable?(name)
            @mvars[name]
        elsif has_variable?("::" + name)
            @mvars["::" + name]
        elsif has_variable?(@context + "::" + name)
            @mvars[@context + "::" + name]
        else
            throw("Unknown variable " + name)
        end
    end

    def has_variable?(name)
        @mvars.include?(name)
    end
end

class ErbBinding
    def initialize(scp, ctx)
        @scope = Scope.new(scp, ctx)
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

context = $stdin.readline.chomp!
templatefile = $stdin.readline.chomp!
varscope = $stdin.read
content = IO.read(templatefile)

nerb = ERB.new(content, nil, "-")
binding = ErbBinding.new(varscope, context).get_binding

out = nerb.result(binding)
puts out
