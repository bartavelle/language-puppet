require 'erb'
require 'digest/md5'
require 'yaml'

class Scope
    def initialize(context,variables,filename)
        @context = context
        @variables = variables
        @file = filename
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

    def function_versioncmp(args)
        version_a = args[0]
        version_b = args[1]
        vre = /[-.]|\d+|[^-.\d]+/
            ax = version_a.scan(vre)
        bx = version_b.scan(vre)

        while (ax.length>0 && bx.length>0)
            a = ax.shift
            b = bx.shift

            if( a == b )                 then next
            elsif (a == '-' && b == '-') then next
            elsif (a == '-')             then return -1
            elsif (b == '-')             then return 1
            elsif (a == '.' && b == '.') then next
            elsif (a == '.' )            then return -1
            elsif (b == '.' )            then return 1
            elsif (a =~ /^\d+$/ && b =~ /^\d+$/) then
                if( a =~ /^0/ or b =~ /^0/ ) then
                    return a.to_s.upcase <=> b.to_s.upcase
                end
                return a.to_i <=> b.to_i
            else
                return a.upcase <=> b.upcase
            end
        end
        version_a <=> version_b;
    end
end

class ErbBinding
    @options = {}
    def initialize(context,variables,filename='x')
        @scope = Scope.new(context,variables,filename)
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
        nerb.result(binding.get_binding)
    end
end

