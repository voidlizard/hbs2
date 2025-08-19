test:root temp
test:dir:keep
set! w:getblk 100
set! w:storm  2
set! w:putblk 90
set! w:blk 65536

println "w:blk" w:blk
println "go"

; test:ncq3:endurance:inproc 200000
test:ncq3:endurance:inproc 300000







