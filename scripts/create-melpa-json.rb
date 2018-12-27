require 'net/http'
require 'uri'
require "json"

# waiting merge pull-request
# https://github.com/melpa/melpa/pull/5888
archives = Net::HTTP.get(URI.parse('https://melpa.org/archive.json'))
recipes = Net::HTTP.get(URI.parse('https://files.conao3.com/feather-recipes/melpa-recipes.json'))

hash_archives = JSON.parse(archives)
hash_recipes = JSON.parse(recipes)
hash = hash_archives.merge(hash_recipes) {|key, v1, v2| v1.merge(v2)}

puts JSON.dump(hash)
