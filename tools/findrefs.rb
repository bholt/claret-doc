#!/usr/bin/env ruby
require 'bibtex'

citekeys = Dir.glob("#{Dir.pwd}/**/*.mdk")
              .map{|f| IO.read(f).scan(/\[@(.+?)\]/) }.flatten
              .map{|s| s.split(";@") }.flatten
              .uniq
              .delete_if{|k| k =~ /^(fig|tab|listing)$/ }

f = "#{Dir.pwd}/refs.bib"
proj_bib = File.exist?(f) ? BibTeX.open(f) : BibTeX::Bibliography.new
changed = false

# bib file to search within for missing references
allbib = "#{ENV['HOME']}/Dropbox/all.bib"
if File.exist? allbib
  search_bib = BibTeX.open(allbib) if File.exist? allbib

  citekeys.each do |k|
    p = proj_bib[k]
    s = search_bib[k]
    
    if p and s
      # s has a newer modified date
      if p["date-modified"] and s["date-modified"] and s["date-modified"] > p["date-modified"]
        puts "updating #{k}"
        proj_bib.remove(k)
        proj_bib.add(s)
        changed = true
      end
    elsif !p and s
      puts "adding #{k}"
      proj_bib.add(s)
      changed = true
    elsif !p and !s
      $stderr.write("warning: unable to find citekey: #{k}\n")
    end
  end
  
  if changed
    # remove BibDesk fields
    proj_bib.each do |e|
      fields = e.to_hash.keys.keep_if{|k| k =~ /^bdsk/}
      if not fields.empty?
        fields.each{|f| e.delete(f) }
      end
    end
    # save back to file 'refs.bib' file
    proj_bib.save_to(f)
  end
  
end