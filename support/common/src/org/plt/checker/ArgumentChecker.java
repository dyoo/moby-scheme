package org.plt.checker;

import org.plt.types.*;

public class ArgumentChecker {
	public static void checkArrayType(Object[] args, Class cl, String funName) {
		for (int i = 0; i < args.length; i++) {
			if (cl.isInstance(args[i]) == false) {
				String err = funName + ": expects type <" + cl.getName()
						+ "> as argument number " + (i + 1) + ", given: "
						+ args[i] + "; other arguments were ";
				for (int j = 0; j < args.length; j++)
					if (j != i)
						err += (args[j] + " ");
				throw new SchemeException(err);
			}
		}
	}

	/**
	 * assumption: arr.length >= 2
	 */
	public static boolean checkArrayType(Object[] arr, RelationChecker rc) {
		Object prev = arr[0];

		for (int i = 1; i < arr.length; i++) {
			if (rc.hasRelation(prev, arr[i]) == false)
				return false;
			prev = arr[i];
		}

		return true;
	}

	public static void checkListType(Object lst, Class cl, String funName) {
		String err = funName + ": expects argument of type <list of "
				+ cl.getName() + ">, given: " + lst;

		if (lst instanceof org.plt.types.List == false)
			throw new SchemeException(err);

		while (((org.plt.types.List) lst).isEmpty() == false) {
			if (cl.isInstance(((org.plt.types.List) lst).first()) == false)
				throw new SchemeException(err);

			lst = ((org.plt.types.List) lst).rest();
		}
	}

	public static void checkListType(Object lst, Class cl, String funName,
			int argNum) {
		String err = funName + ": expects argument number " + argNum
				+ " of type <list of " + cl.getName() + ">, given: " + lst;

		if (lst instanceof org.plt.types.List == false)
			throw new SchemeException(err);

		while (((org.plt.types.List) lst).isEmpty() == false) {
			if (cl.isInstance(((org.plt.types.List) lst).first()) == false)
				throw new SchemeException(err);

			lst = ((org.plt.types.List) lst).rest();
		}
	}

	public static void checkArraySize(Object[] args, int sizeLowerBound,
			String funName) {
		if (args.length < sizeLowerBound)
			throw new SchemeException(funName + ": expects at least "
					+ sizeLowerBound + " arguments, given " + args.length);
	}

	public static void checkAtomType(Object n, String desiredType,
			String funName) {
		try {
			Class cl = Class.forName(desiredType);
			if (cl.isInstance(n) == false) {
				String err = funName + ": expects argument of type <"
						+ desiredType + ">; given " + n;
				throw new SchemeException(err);
			}
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}
	}

	public static void checkAtomType(Object n, PropertyChecker checker,
			String propertyType, String funName, int argNum) {

		if (!checker.satisfied(n))
			throw new SchemeException(funName + ": expects argument number "
					+ argNum + " of type <" + propertyType + ">; given " + n);
	}

	public static void checkAtomType(Object n, PropertyChecker checker,
			String propertyType, String funName) {

		if (!checker.satisfied(n))
			throw new SchemeException(funName + ": expects argument of type <"
					+ propertyType + ">; given " + n);
	}
}
