#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys

if len( sys.argv ) != 2:
    print("   *** Usage: %s <input_filename>" % sys.argv[0] )
    exit( 1 )

f = open( sys.argv[1], 'r' )

# NP ND : Number of Providers, Number of Demands
NP, ND = -1, -1

# Every item here is an offer given by an Provider
O = []

# Every item here is a Demand made on the solicitation
D = []

provider_number = 0
header = True
for line in f:

    # Parsing the reader -----------------------
    if header == True:
        header = False
        L = line.strip().split()
        NP, ND = int( L[0] ), int(L[1])
        continue
        
    # Now, every line is a provider ------------
    L = line.strip().replace('{', '').replace('}', '').replace(' ', '')
    
    # Every part separated by an ';' is an Offer
    for i, off_txt in enumerate(L.split( ';' )):
        cover, cost = off_txt.split( ':' )
        cover = list( map( int, cover.split(',') ))
        
        offer = {}
        offer[ 'provider' ] = provider_number
        offer[ 'name' ] = 'o_%d_%d' % ( provider_number, i )
        offer[ 'cost' ] = cost
        offer[ 'cover' ] = cover
        
        O.append( offer )


    provider_number = provider_number + 1

D = [ { 'name': 'd_%d' % x } for x in range(ND) ]

# Done reading the input file and preparing the necessary data structures: NP, ND, O, D
# Printing LP
VARS = {}

print('MIN')
for offer in O:
    print( '+ %s x_s_%s' %(offer['cost'], offer['name'] ) )
    VARS[ 'x_s_%s' % offer['name']  ] = 1

print('\nST')

# All demands should be supplied (sum( leaves ) s == ND)
for offer in O:
    print( '+x_s_%s' % offer['name'], end=' '  )
print(' = %d\n' % ND )


# Flow conservation in every offer
for offer in O:
    print( '-x_s_%s' % offer['name'], end=' '  )

    for c in offer['cover']:
        print( '+x_%s_d_%s' % (offer['name'], c), end=' '  )
        VARS[ 'x_%s_d_%s' % (offer['name'], c)  ] = 1
    
    print(' = 0' )
print( '' )

# Every demand gets exactly one offer
for d in range(ND):
    for offer in O:
        if d in offer['cover']:
            print( '+x_%s_d_%d' % (offer['name'], d), end=' '  )
            VARS[ 'x_%s_d_%d' % (offer['name'], d)  ] = 1
    print(' = 1' )

# Bounds ----------------------------    
print('\nBOUNDS')
for v in VARS:
    print('0 <= %s <= 1' % v)
    
    
print('\nEND')
