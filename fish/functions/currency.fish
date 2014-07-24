function currency --description 'Uses Google Finance to convert currency' --argument amount from to
	wget -qO- "http://www.google.com/finance/converter?a=$amount&from=$from&to=$to&hl=es" |  sed '/res/!d;s/<[^>]*>//g';
end
