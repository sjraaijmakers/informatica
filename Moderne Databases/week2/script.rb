# Steven Raaijmakers,10804242
# pattenrs where made bymeeskalf

require 'couchrest'

# Database conenction
@db = CouchRest.database!("http://localhost:5984/ruby_books")

# Strip value to a word
def strip(value)
    if (value.to_s)["<rdfs:label>"]
        return value.to_s[/#{'<rdfs:label>'}(.*?)#{'</rdfs:label>'}/m, 1]
    elsif !value.nil?
        # skip skos:notation (gives weird output)
        if value.to_s.include? "skos:notation"
            return nil
        else
            return value[0].to_s
        end
    end
end

# Determine wheter the new value has to be added to a list,
# or create new key, value pair
def make_pair(key, value, book)
    # first occurrence of key
    if !book.key?(key)
        book[key] = value
    # key aready in hash; create list if key only has one value yet
    else
        if !book[key].kind_of?(Array)
            book[key] = [book[key]].push(value)
        else
            book[key].push(value)
        end
    end
end

# Create json object from rdf lines
def lines_to_hash(lines)
    book = {}

    # Get the array of keys of the book
    tags = lines.scan(/#{'<dcterms:'}(.*?)#{'>'}/m).uniq
    tags += lines.scan(/#{'<rda:'}(.*?)#{'>'}/m).uniq

    tags.each do |tag|
        key = tag[0]
        values = lines.scan(/#{"<dcterms:#{key}>(.*?)</dcterms:#{key}>"}/m)
        values += lines.scan(/#{"<rda:#{key}>(.*?)</rda:#{key}>"}/m)

        # for all found values
        values.each do |value|
            # clean value to an english word
            value = strip(value)
            # determine where to put the new value
            if !value.nil?
                make_pair(key, value, book)
            end
        end
    end
    # puts book
    @db.save_doc(book)
end



tmp = ''
file = File.open("k.rdf")
file.each_line do |line|
    unless line.include? '?xml' or line.include? 'rdf:RDF'
        if line['<']
            tmp += line
        elsif !tmp.empty?
            lines_to_hash(tmp)
            tmp.clear
        end
    end
end
