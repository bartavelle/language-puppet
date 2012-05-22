require 'erb'

class Scope
    def initialize(scpv)
        @vars = scpv
    end

    def lookupvar(name)
        @vars[name]
    end
end

class ErbBinding
    def initialize(scp)
        @scope = Scope.new(scp)
    end
    def get_binding
        return binding()
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

content = File.read("/tmp/test.erb")
varscope = { 'lapin' => 'malin' }
out = ERB.new(content).result(ErbBinding.new(varscope).get_binding)
puts out
