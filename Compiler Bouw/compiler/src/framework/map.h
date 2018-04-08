#ifndef _SAC_MAP_H_
#define _SAC_MAP_H_

#include "types.h"

typedef struct MAP map;

extern map *MAPcreate( bool (*eq)(void *, void *));
extern map *MAPcopy( map *map);
extern map *MAPdelete( map *map);
extern map *MAPclear( map *map);
extern bool MAPisEmpty( map *map);
extern map *MAPinsert( map *map, void *key, void *item);
extern void *MAPlookup(  map *map, void *key);
extern map *MAPupdate( map *map, void *key, void *item);
extern map *MAPswap( map *map, void *key, void *item, void **found);
extern void MAPmap( map *map, void *(*mapfun)( void *));
extern void *MAPfold( map *map, void *start, void *(*foldfun)( void *, void *));

#endif /* _SAC_MAP_H_ */
