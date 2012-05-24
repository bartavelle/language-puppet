require 'erb'

class Scope
    def initialize(scpv)
        @mvars = eval(scpv)
    end

    def lookupvar(name)
        if has_variable?(name)
            @mvars[name]
        elsif has_variable?("::" + name)
            @mvars["::" + name]
        else
            @mvars[@context ++ "::" ++ name]
        end
    end

    def has_variable?(name)
        @mvars.include?(name)
    end
end

class ErbBinding
    def initialize(scp)
        @scope = Scope.new(scp)
    end
    def get_binding
        return binding()
    end
    def has_variable?(name)
        @scope.has_variable?(name.to_s)
    end
    def method_missing(sname)
        puts sname
        name = sname.to_s
        if name == 'scope'
            @scope
        else
            @scope.lookupvar(name)
        end
    end
end

context = $stdin.readline
templatefile = $stdin.readline.chomp!
varscope = $stdin.read
content = IO.read(templatefile)

nerb = ERB.new(content, nil, "-")
binding = ErbBinding.new(varscope).get_binding

out = nerb.result(binding)
puts out
