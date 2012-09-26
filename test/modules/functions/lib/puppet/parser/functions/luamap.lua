function luamap(args)
    out = {}
    i = 1
    for k,v in pairs(args[1]) do
        out[i] = k*2
        i = i + 1
    end
    return out
end

