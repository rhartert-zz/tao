package tao;

import tao.util.Pair;

public class TAOInstance {
	
	public final int nAssistants;
	public final int nCourses;
	public final int[] maxHours;
	public final int[] hours;
	public final int[][] courseAssistants;
	public final Pair[] required;
	public final Pair[] forbidden;
	public final int[] oldAssignments;
	
	public TAOInstance(int[] maxHours, int[] hours, int[][] courseAssistants, Pair[] required, Pair[] forbidden, int[] oldAssignments) {
		this.nAssistants = maxHours.length;
		this.nCourses = courseAssistants.length;
		this.maxHours = maxHours;
		this.hours = hours;
		this.courseAssistants = courseAssistants;
		this.required = required;
		this.forbidden = forbidden;
		this.oldAssignments = oldAssignments;
	}
}

