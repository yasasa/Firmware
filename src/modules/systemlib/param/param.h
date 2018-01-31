/****************************************************************************
 *
 *   Copyright (c) 2012-2015 PX4 Development Team. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 * 3. Neither the name PX4 nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 ****************************************************************************/

/**
 * @file param.h
 *
 * Global parameter store.
 *
 * Note that a number of API members are marked const or pure; these
 * assume that the set of parameters cannot change, or that a parameter
 * cannot change type or size over its lifetime.  If any of these assumptions
 * are invalidated, the attributes should be re-evaluated.
 */

#ifndef _SYSTEMLIB_PARAM_PARAM_H
#define _SYSTEMLIB_PARAM_PARAM_H

#include <stdint.h>
#include <stdbool.h>
#include <sys/types.h>

/** Maximum size of the parameter backing file */
#define PARAM_FILE_MAXSIZE		4096

__BEGIN_DECLS

/**
 * Parameter types.
 */
#define PARAM_TYPE_INT32		0
#define PARAM_TYPE_FLOAT		1
#define PARAM_TYPE_STRUCT		100
#define PARAM_TYPE_STRUCT_MAX	(16384 + PARAM_TYPE_STRUCT)
#define PARAM_TYPE_UNKNOWN		(0xffff)

typedef uint16_t param_type_t;


#ifdef __PX4_NUTTX // on NuttX use 16 bits to save RAM
/**
 * Parameter handle.
 *
 * Parameters are represented by parameter handles, which can
 * be obtained by looking up parameters. They are an offset into a global
 * constant parameter array.
 */
typedef uint16_t	param_t;

/**
 * Handle returned when a parameter cannot be found.
 */
#define PARAM_INVALID	((uint16_t)0xffff)

/**
 * Magic handle for hash check param
 */
#define PARAM_HASH      ((uint16_t)INT16_MAX)

#else // on other platforms use 32 bits for better performance

/**
 * Parameter handle.
 *
 * Parameters are represented by parameter handles, which can
 * be obtained by looking up parameters. They are an offset into a global
 * constant parameter array.
 */
typedef uint32_t	param_t;

/**
 * Handle returned when a parameter cannot be found.
 */
#define PARAM_INVALID	((uint32_t)0xffffffff)

/**
 * Magic handle for hash check param
 */
#define PARAM_HASH      ((uint32_t)INT32_MAX)

#endif /* __PX4_NUTTX */


/**
 * Initialize the param backend. Call this on startup before calling any other methods.
 */
__EXPORT void		param_init(void);

/**
 * Look up a parameter by name.
 *
 * @param name		The canonical name of the parameter being looked up.
 * @return		A handle to the parameter, or PARAM_INVALID if the parameter does not exist.
 *			This call will also set the parameter as "used" in the system, which is used
 *			to e.g. show the parameter via the RC interface
 */
__EXPORT param_t	param_find(const char *name);

/**
 * Look up a parameter by name.
 *
 * @param name		The canonical name of the parameter being looked up.
 * @return		A handle to the parameter, or PARAM_INVALID if the parameter does not exist.
 */
__EXPORT param_t	param_find_no_notification(const char *name);

/**
 * Return the total number of parameters.
 *
 * @return		The number of parameters.
 */
__EXPORT unsigned	param_count(void);

/**
 * Return the actually used number of parameters.
 *
 * @return		The number of parameters.
 */
__EXPORT unsigned	param_count_used(void);

/**
 * Wether a parameter is in use in the system.
 *
 * @return		True if it has been written or read
 */
__EXPORT bool		param_used(param_t param);

/**
 * Look up a parameter by index.
 *
 * @param index		An index from 0 to n, where n is param_count()-1.
 * @return		A handle to the parameter, or PARAM_INVALID if the index is out of range.
 */
__EXPORT param_t	param_for_index(unsigned index);

/**
 * Look up an used parameter by index.
 *
 * @param index		The parameter to obtain the index for.
 * @return		The index of the parameter in use, or -1 if the parameter does not exist.
 */
__EXPORT param_t	param_for_used_index(unsigned index);

/**
 * Look up the index of a parameter.
 *
 * @param param		The parameter to obtain the index for.
 * @return		The index, or -1 if the parameter does not exist.
 */
__EXPORT int		param_get_index(param_t param);

/**
 * Look up the index of an used parameter.
 *
 * @param param		The parameter to obtain the index for.
 * @return		The index of the parameter in use, or -1 if the parameter does not exist.
 */
__EXPORT int		param_get_used_index(param_t param);

/**
 * Obtain the name of a parameter.
 *
 * @param param		A handle returned by param_find or passed by param_foreach.
 * @return		The name assigned to the parameter, or NULL if the handle is invalid.
 */
__EXPORT const char	*param_name(param_t param);

/**
 * Obtain the volatile state of a parameter.
 *
 * @param param		A handle returned by param_find or passed by param_foreach.
 * @return			true if the parameter is volatile
 */
__EXPORT bool		param_is_volatile(param_t param);

/**
 * Test whether a parameter's value has changed from the default.
 *
 * @return		If true, the parameter's value has not been changed from the default.
 */
__EXPORT bool		param_value_is_default(param_t param);

/**
 * Test whether a parameter's value has been changed but not saved.
 *
 * @return		If true, the parameter's value has not been saved.
 */
__EXPORT bool		param_value_unsaved(param_t param);

/**
 * Obtain the type of a parameter.
 *
 * @param param		A handle returned by param_find or passed by param_foreach.
 * @return		The type assigned to the parameter.
 */
__EXPORT param_type_t	param_type(param_t param);

/**
 * Determine the size of a parameter.
 *
 * @param param		A handle returned by param_find or passed by param_foreach.
 * @return		The size of the parameter's value.
 */
__EXPORT size_t		param_size(param_t param);

/**
 * Copy the value of a parameter.
 *
 * @param param		A handle returned by param_find or passed by param_foreach.
 * @param val		Where to return the value, assumed to point to suitable storage for the parameter type.
 *			For structures, a bitwise copy of the structure is performed to this address.
 * @return		Zero if the parameter's value could be returned, nonzero otherwise.
 */
__EXPORT int		param_get(param_t param, void *val);

/**
 * Set the value of a parameter.
 *
 * @param param		A handle returned by param_find or passed by param_foreach.
 * @param val		The value to set; assumed to point to a variable of the parameter type.
 *			For structures, the pointer is assumed to point to a structure to be copied.
 * @return		Zero if the parameter's value could be set from a scalar, nonzero otherwise.
 */
__EXPORT int		param_set(param_t param, const void *val);

/**
 * Mark a parameter as used. Only marked parameters will be sent to a GCS.
 * A call to param_find() will mark a param as used as well.
 *
 * @param param		A handle returned by param_find or passed by param_foreach.
 */
__EXPORT void		param_set_used(param_t param);

/**
 * Set the value of a parameter, but do not notify the system about the change.
 *
 * @param param		A handle returned by param_find or passed by param_foreach.
 * @param val		The value to set; assumed to point to a variable of the parameter type.
 *			For structures, the pointer is assumed to point to a structure to be copied.
 * @return		Zero if the parameter's value could be set from a scalar, nonzero otherwise.
 */
__EXPORT int		param_set_no_notification(param_t param, const void *val);

/**
 * Notify the system about parameter changes. Can be used for example after several calls to
 * param_set_no_notification() to avoid unnecessary system notifications.
 */
__EXPORT void		param_notify_changes(void);

/**
 * Reset a parameter to its default value.
 *
 * This function frees any storage used by struct parameters, and returns the parameter
 * to its default value.
 *
 * @param param		A handle returned by param_find or passed by param_foreach.
 * @return		Zero on success, nonzero on failure
 */
__EXPORT int		param_reset(param_t param);

/**
 * Reset all parameters to their default values.
 *
 * This function also releases the storage used by struct parameters.
 */
__EXPORT void		param_reset_all(void);

/**
 * Reset all parameters to their default values except for excluded parameters.
 *
 * This function also releases the storage used by struct parameters.
 *
 * @param excludes			Array of param names to exclude from resetting. Use a wildcard
 *							at the end to exclude parameters with a certain prefix.
 * @param num_excludes		The number of excludes provided.
 */
__EXPORT void		param_reset_excludes(const char *excludes[], int num_excludes);

/**
 * Export changed parameters to a file.
 * Note: this method requires a large amount of stack size!
 *
 * @param fd		File descriptor to export to.
 * @param only_unsaved	Only export changed parameters that have not yet been exported.
 * @return		Zero on success, nonzero on failure.
 */
__EXPORT int		param_export(int fd, bool only_unsaved);

/**
 * Import parameters from a file, discarding any unrecognized parameters.
 *
 * This function merges the imported parameters with the current parameter set.
 *
 * @param fd		File descriptor to import from.  (Currently expected to be a file.)
 * @return		Zero on success, nonzero if an error occurred during import.
 *			Note that in the failure case, parameters may be inconsistent.
 */
__EXPORT int		param_import(int fd);

/**
 * Load parameters from a file.
 *
 * This function resets all parameters to their default values, then loads new
 * values from a file.
 *
 * @param fd		File descriptor to import from.  (Currently expected to be a file.)
 * @return		Zero on success, nonzero if an error occurred during import.
 *			Note that in the failure case, parameters may be inconsistent.
 */
__EXPORT int		param_load(int fd);

/**
 * Apply a function to each parameter.
 *
 * Note that the parameter set is not locked during the traversal. It also does
 * not hold an internal state, so the callback function can block or sleep between
 * parameter callbacks.
 *
 * @param func		The function to invoke for each parameter.
 * @param arg		Argument passed to the function.
 * @param only_changed	If true, the function is only called for parameters whose values have
 *			been changed from the default.
 * @param only_changed	If true, the function is only called for parameters which have been
 *			used in one of the running applications.
 */
__EXPORT void		param_foreach(void (*func)(void *arg, param_t param), void *arg, bool only_changed, bool only_used);

/**
 * Set the default parameter file name.
 *
 * @param filename	Path to the default parameter file.  The file is not require to
 *			exist.
 * @return		Zero on success.
 */
__EXPORT int 		param_set_default_file(const char *filename);

/**
 * Get the default parameter file name.
 *
 * @return		The path to the current default parameter file; either as
 *			a result of a call to param_set_default_file, or the
 *			built-in default.
 */
__EXPORT const char	*param_get_default_file(void);

/**
 * Save parameters to the default file.
 * Note: this method requires a large amount of stack size!
 *
 * This function saves all parameters with non-default values.
 *
 * @return		Zero on success.
 */
__EXPORT int 		param_save_default(void);

/**
 * Load parameters from the default parameter file.
 *
 * @return		Zero on success.
 */
__EXPORT int 		param_load_default(void);

/**
 * Generate the hash of all parameters and their values
 *
 * @return		CRC32 hash of all param_ids and values
 */
__EXPORT uint32_t	param_hash_check(void);


/**
 * Enable/disable the param autosaving.
 * Re-enabling with changed params will not cause an autosave.
 * @param enable true: enable autosaving, false: disable autosaving
 */
__EXPORT void	param_control_autosave(bool enable);

/*
 * Macros creating static parameter definitions.
 *
 * Note that these structures are not known by name; they are
 * collected into a section that is iterated by the parameter
 * code.
 *
 * Note that these macros cannot be used in C++ code due to
 * their use of designated initializers.  They should probably
 * be refactored to avoid the use of a union for param_value_u.
 */

/** define an int32 parameter */
#define PARAM_DEFINE_INT32(_name, _default)

/** define a float parameter */
#define PARAM_DEFINE_FLOAT(_name, _default)

/** define a parameter that points to a structure */
#define PARAM_DEFINE_STRUCT(_name, _default)

/**
 * Parameter value union.
 */
union param_value_u {
	void		*p;
	int32_t		i;
	float		f;
};

/**
 * Static parameter definition structure.
 *
 * This is normally not used by user code; see the PARAM_DEFINE macros
 * instead.
 */
struct param_info_s {
	const char	*name

// GCC 4.8 and higher don't implement proper alignment of static data on
// 64-bit. This means that the 24-byte param_info_s variables are
// 16 byte aligned by GCC and that messes up the assumption that
// sequential items in the __param segment can be addressed as an array.
// The assumption is that the address of the second parameter is at
// &param[0]+sizeof(param[0]). When compiled with clang it is
// true, with gcc is is not true.
// See https://llvm.org/bugs/show_bug.cgi?format=multiple&id=18006
// The following hack is for GCC >=4.8 only. Clang works fine without
// this.
#ifdef __PX4_POSIX
	__attribute__((aligned(16)));
#else
	;
#endif
	param_type_t	type;
	uint16_t		volatile_param: 1;
	union param_value_u val;
};

__END_DECLS



#ifdef	__cplusplus
#if 0 // set to 1 to debug param type mismatches
#include <cstdio>
#define CHECK_PARAM_TYPE(param, type) \
	if (param_type(param) != type) { \
		/* use printf() to avoid having to use more includes */ \
		printf("wrong type passed to param_get() for param %s\n", param_name(param)); \
	}
#else
#define CHECK_PARAM_TYPE(param, type)
#endif

// param is a C-interface. This means there is no overloading, and thus no type-safety for param_get().
// So for C++ code we redefine param_get() to inlined overloaded versions, which gives us type-safety
// w/o having to use a different interface
static inline int param_get_cplusplus(param_t param, float *val)
{
	CHECK_PARAM_TYPE(param, PARAM_TYPE_FLOAT);
	return param_get(param, val);
}
static inline int param_get_cplusplus(param_t param, int32_t *val)
{
	CHECK_PARAM_TYPE(param, PARAM_TYPE_INT32);
	return param_get(param, val);
}
#undef CHECK_PARAM_TYPE

#define param_get(param, val) param_get_cplusplus(param, val)





#define APPLY0(t)
#define APPLY1(t, aaa) t(aaa)
#define APPLY2(t, aaa, aab) t(aaa) t(aab)
#define APPLY3(t, aaa, aab, aac) t(aaa) t(aab) t(aac)
#define APPLY4(t, aaa, aab, aac, aad) t(aaa) t(aab) t(aac) t(aad)
#define APPLY5(t, aaa, aab, aac, aad, aae) t(aaa) t(aab) t(aac) t(aad) t(aae)
#define APPLY6(t, aaa, aab, aac, aad, aae, aaf) t(aaa) t(aab) t(aac) t(aad) t(aae) t(aaf)
#define APPLY7(t, aaa, aab, aac, aad, aae, aaf, aag) t(aaa) t(aab) t(aac) t(aad) t(aae) t(aaf) t(aag)
#define APPLY8(t, aaa, aab, aac, aad, aae, aaf, aag, aah) t(aaa) t(aab) t(aac) t(aad) t(aae) t(aaf) t(aag) t(aah)
#define APPLY9(t, aaa, aab, aac, aad, aae, aaf, aag, aah, aai) t(aaa) t(aab) t(aac) t(aad) t(aae) t(aaf) t(aag) t(aah) t(aai)
#define APPLY10(t, aaa, aab, aac, aad, aae, aaf, aag, aah, aai, aaj) t(aaa) t(aab) t(aac) t(aad) t(aae) t(aaf) t(aag) t(aah) t(aai) t(aaj)
#define APPLY11(t, aaa, aab, aac, aad, aae, aaf, aag, aah, aai, aaj, aak) t(aaa) t(aab) t(aac) t(aad) t(aae) t(aaf) t(aag) t(aah) t(aai) t(aaj) t(aak)
#define APPLY12(t, aaa, aab, aac, aad, aae, aaf, aag, aah, aai, aaj, aak, aal) t(aaa) t(aab) t(aac) t(aad) t(aae) t(aaf) t(aag) t(aah) t(aai) t(aaj) t(aak) t(aal)
#define APPLY13(t, aaa, aab, aac, aad, aae, aaf, aag, aah, aai, aaj, aak, aal, aam) t(aaa) t(aab) t(aac) t(aad) t(aae) t(aaf) t(aag) t(aah) t(aai) t(aaj) t(aak) t(aal) t(aam)
#define APPLY14(t, aaa, aab, aac, aad, aae, aaf, aag, aah, aai, aaj, aak, aal, aam, aan) t(aaa) t(aab) t(aac) t(aad) t(aae) t(aaf) t(aag) t(aah) t(aai) t(aaj) t(aak) t(aal) t(aam) t(aan)
#define APPLY15(t, aaa, aab, aac, aad, aae, aaf, aag, aah, aai, aaj, aak, aal, aam, aan, aao) t(aaa) t(aab) t(aac) t(aad) t(aae) t(aaf) t(aag) t(aah) t(aai) t(aaj) t(aak) t(aal) t(aam) t(aan) t(aao)
#define APPLY16(t, aaa, aab, aac, aad, aae, aaf, aag, aah, aai, aaj, aak, aal, aam, aan, aao, aap) t(aaa) t(aab) t(aac) t(aad) t(aae) t(aaf) t(aag) t(aah) t(aai) t(aaj) t(aak) t(aal) t(aam) t(aan) t(aao) t(aap)
#define APPLY17(t, aaa, aab, aac, aad, aae, aaf, aag, aah, aai, aaj, aak, aal, aam, aan, aao, aap, aaq) t(aaa) t(aab) t(aac) t(aad) t(aae) t(aaf) t(aag) t(aah) t(aai) t(aaj) t(aak) t(aal) t(aam) t(aan) t(aao) t(aap) t(aaq)
#define APPLY18(t, aaa, aab, aac, aad, aae, aaf, aag, aah, aai, aaj, aak, aal, aam, aan, aao, aap, aaq, aar) t(aaa) t(aab) t(aac) t(aad) t(aae) t(aaf) t(aag) t(aah) t(aai) t(aaj) t(aak) t(aal) t(aam) t(aan) t(aao) t(aap) t(aaq) t(aar)
#define APPLY19(t, aaa, aab, aac, aad, aae, aaf, aag, aah, aai, aaj, aak, aal, aam, aan, aao, aap, aaq, aar, aas) t(aaa) t(aab) t(aac) t(aad) t(aae) t(aaf) t(aag) t(aah) t(aai) t(aaj) t(aak) t(aal) t(aam) t(aan) t(aao) t(aap) t(aaq) t(aar) t(aas)

#define NUM_ARGS_H1(dummy, x19, x18, x17, x16, x15, x14, x13, x12, x11, x10, x9, x8, x7, x6, x5, x4, x3, x2, x1, x0, ...) x0
#define NUM_ARGS(...) NUM_ARGS_H1(dummy, ##__VA_ARGS__, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)

#define APPLY_ALL_H3(t, n, ...) APPLY##n(t, __VA_ARGS__)
#define APPLY_ALL_H2(t, n, ...) APPLY_ALL_H3(t, n, __VA_ARGS__)
#define APPLY_ALL(t, ...) APPLY_ALL_H2(t, NUM_ARGS(__VA_ARGS__), __VA_ARGS__)


// TODO: add proper include path...
#include <modules/systemlib/param/px4_parameters_public.h>



// helper macros to handle macro arguments in the form: (type) name

#define REM(...) __VA_ARGS__
#define EAT(...)

// Retrieve the type
#define TYPEOF(x) DETAIL_TYPEOF(DETAIL_TYPEOF_PROBE x,)
#define DETAIL_TYPEOF(...) DETAIL_TYPEOF_HEAD(__VA_ARGS__)
#define DETAIL_TYPEOF_HEAD(x, ...) REM x
#define DETAIL_TYPEOF_PROBE(...) (__VA_ARGS__),
// Strip off the type, get the name
#define STRIP(x) EAT x
// Show the type without parenthesis
#define PAIR(x) REM x



#define _DEFINE_SINGLE_PARAMETER(x) \
	do_not_explicitly_use_this_namespace::PAIR(x);

#define _CALL_UPDATE(x) \
	STRIP(x).update();

// define the parameter update method, which will update all parameters.
// It is marked as 'final', so that wrong usages lead to a compile error (see below)
#define _DEFINE_PARAMETER_UPDATE_METHOD(...) \
	protected: \
	void updateParamsSubclass() final { \
		APPLY_ALL(_CALL_UPDATE, __VA_ARGS__) \
	} \
	private:

// Define a list of block parameters. This macro also creates code to update parameters.
// If you get a compile error like:
//   error: virtual function ‘virtual void <class>::updateParamsSubclass()’
// It means you have a custom inheritance tree (at least one class with params that inherits from another
// class with params) and you need to use DEFINE_BLOCK_PARAMETERS_CUSTOM_PARENT() for **all** classes in
// that tree.
#define DEFINE_BLOCK_PARAMETERS(...) \
	APPLY_ALL(_DEFINE_SINGLE_PARAMETER, __VA_ARGS__) \
	_DEFINE_PARAMETER_UPDATE_METHOD(__VA_ARGS__)


#define _DEFINE_PARAMETER_UPDATE_METHOD_CUSTOM_PARENT(parent_class, ...) \
	protected: \
	void updateParamsSubclass() { \
		parent_class::updateParamsSubclass(); \
		APPLY_ALL(_CALL_UPDATE, __VA_ARGS__) \
	} \
	private:

#define DEFINE_BLOCK_PARAMETERS_CUSTOM_PARENT(parent_class, ...) \
	APPLY_ALL(_DEFINE_SINGLE_PARAMETER, __VA_ARGS__) \
	_DEFINE_PARAMETER_UPDATE_METHOD_CUSTOM_PARENT(parent_class, __VA_ARGS__)



inline static param_t param_handle(px4::params p)
{
	return (param_t)p;
}

// This namespace never needs to be used directly. Use the DEFINE_BLOCK_PARAMETERS_CUSTOM_PARENT and
// DEFINE_BLOCK_PARAMETERS macros instead.
namespace do_not_explicitly_use_this_namespace
{

template<typename T, px4::params p>
class BlockParam
{
};

// We use partial template specialization for each param type. This is only supported for classes, not individual methods,
// which is why we have to repeat the whole class
template<px4::params p>
class BlockParam<float, p>
{
public:
	// static type-check
	static_assert(px4::param_types_array[(int)p] == PARAM_TYPE_FLOAT, "parameter type must be float");

	BlockParam()
	{
		param_set_used(handle());
		update();
	}

	float get() const { return _val; }

	/// Store the parameter value to the parameter storage (@see param_set())
	bool commit() const { return param_set(handle(), &_val) == 0; }

	/// Store the parameter value to the parameter storage, w/o notifying the system (@see param_set_no_notification())
	bool commit_no_notification() const { return param_set_no_notification(handle(), &_val) == 0; }

	void set(float val) { _val = val; }

	bool update() { return param_get(handle(), &_val) == 0; }

	param_t handle() const { return param_handle(p); }
private:
	float _val;
};

template<px4::params p>
class BlockParam<int32_t, p>
{
public:
	// static type-check
	static_assert(px4::param_types_array[(int)p] == PARAM_TYPE_INT32, "parameter type must be int32_t");

	BlockParam()
	{
		param_set_used(handle());
		update();
	}

	int32_t get() const { return _val; }

	/// Store the parameter value to the parameter storage (@see param_set())
	bool commit() const { return param_set(handle(), &_val) == 0; }

	/// Store the parameter value to the parameter storage, w/o notifying the system (@see param_set_no_notification())
	bool commit_no_notification() const { return param_set_no_notification(handle(), &_val) == 0; }

	void set(int32_t val) { _val = val; }

	bool update() { return param_get(handle(), &_val) == 0; }

	param_t handle() const { return param_handle(p); }
private:
	int32_t _val;
};

// TODO: add bool type

template <px4::params p>
using BlockParamFloat = BlockParam<float, p>;

template <px4::params p>
using BlockParamInt = BlockParam<int32_t, p>;


} /* namespace do_not_explicitly_use_this_namespace */


// Raise an appropriate compile error if a BlockParam class is used directly
//template<px4::params p>
//class BlockParamInt
//{
//	static_assert(false && (int)p, "Do not use this class directly, use the DEFINE_BLOCK_PARAMETERS macro instead");
//};

#endif /* __cplusplus */

#endif
