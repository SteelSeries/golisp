ldflags = ""
if RUBY_PLATFORM =~ /darwin/
  ldflags = "-ldflags -linkmode=external"
end

binpath = "bin"
if RUBY_PLATFORM =~ /windows/ and ENV[GOARCH] == "386"
  binpath = "bin/windows_386"
end

system("go get " + ldflags + " github.com/SteelSeries/godep")
system(binpath + "/godep install")
system("go install " + ldflags + " bitbucket.org/dastels/goose-sqlite")
