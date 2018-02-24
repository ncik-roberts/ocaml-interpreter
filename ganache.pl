#!/usr/bin/env perl

use warnings;
use strict;
use Capture::Tiny qw( capture_stdout );
use HTTP::Request;
use LWP::UserAgent;
use JSON::Parse 'parse_json';

# Read compile contract from stdin
my $code = <>;
chomp $code;

my $url = 'http://localhost:8545';

# Open file handle to read from ganache-cli command
open my $fh, 'ganache-cli |';

# Consume output until it has started up
while (<$fh>) {
  last if /Listening on/;
}

my $account_result = send_request('eth_accounts', []);
my $account = $account_result->[0];
print "Account: $account\n";

# Put code on blockchain and find receipt
my $receipt_id = send_request('eth_sendTransaction', [{
  from => $account,
  to => "0x0",
  gas => "0x76c01",
  gasPrice => "0x1",
  data => $code,
}]);

# Find contract id from receipt id
my $receipt_result = send_request('eth_getTransactionReceipt', [ $receipt_id ]);
my $contract = $receipt_result->{ contractAddress };
print "Contract: $contract\n";

# Send call to created contract
my $result = send_request('eth_call', [{
  from => $account,
  to => $contract,
}]);

print "Result: $result\n";

exit;

# Encode an object as a JSON string
sub encode {
  my ($obj) = @_;
  my $type = ref $obj;
  if ($type eq 'HASH') {
    my $body = join ",",
      map { "\"$_\":" . encode($obj->{$_}) } keys %$obj;
    return "{$body}";
  } elsif ($type eq 'ARRAY') {
      my $body = join ",", map { encode($_) } @$obj;
      return "[$body]";
  } else {
    return "\"$obj\"";
  }
}

sub send_request {
  my $ua = LWP::UserAgent->new;

  my ($path, $params) = @_;
  my $req = HTTP::Request->new(POST => "$url/$path");
  $req->content_type('application/json');

  my $content = encode({
    jsonrpc => "2.0",
    method => $path,
    params => $params,
    id => 1,
  });
  $req->content($content);

  my $raw_response = $ua->request( $req )->content;
  print $raw_response, "\n";
  my $response = parse_json( $raw_response );
  return $response->{ result };
}
