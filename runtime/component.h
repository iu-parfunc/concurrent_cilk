/* component.h                  -*-C++-*-
 *
 *************************************************************************
 *
 * Copyright (C) 2009-2011 
 * Intel Corporation
 * 
 * This file is part of the Intel Cilk Plus Library.  This library is free
 * software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * Under Section 7 of GPL version 3, you are granted additional
 * permissions described in the GCC Runtime Library Exception, version
 * 3.1, as published by the Free Software Foundation.
 * 
 * You should have received a copy of the GNU General Public License and
 * a copy of the GCC Runtime Library Exception along with this program;
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
 * <http://www.gnu.org/licenses/>.
 **************************************************************************/

#ifndef INCLUDED_COMPONENT_DOT_H
#define INCLUDED_COMPONENT_DOT_H

#define COMPONENT_NAME "Intel� Cilk� Plus Runtime"

#define COMPONENT_INTERNAL_NAME COMPONENT_NAME

#define COMPONENT_FILENAME "CILKRTS20"

#define BuildVersionString(_major, _minor, _build, _rev) #_major "," #_minor "," #_build "," #_rev

#define COMPONENT_VERSION_STRING BuildVersionString (VERSION_MAJOR, VERSION_MINOR, VERSION_BUILD, VERSION_REVISION)

#endif // ! defined(INCLUDED_COMPONENT_DOT_H)
