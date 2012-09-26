-- this function accepts a numerical argument and adds 1
function hashtolist(args)
    out = {}
    for k,v in pairs(args[1]) do
        out[#out+1] = k
        out[#out+1] = v
        print(k .. " -> " .. v)
    end
    return out
end

