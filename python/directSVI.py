import numpy as np

# # Example --------------------------------------------------------------------
# import matplotlib.pyplot as plt
# x = np.arange(-1, 1, 0.1)
# y = 0.1 + -0.3*(x-0.1) + np.sqrt((x-0.1)**2 + 0.1) + np.random.normal(0, 0.02, 20)
# z = svifit(x, y) # estimate the model
# yfit = svi(x, z) # create fitted y's
# 
# plt.clf()
# plt.scatter(x,y)
# plt.plot(x, yfit)
# plt.show()

# SVI Fit ----------------------------------------------------------------------
def svifit(x,y):
  # (i) fits the direct least-squares method to empirical data
  # (ii) input: x = log-monesness, y = implied variance
  # (iii) returns: the fitted conic coefficients z
  n = len(x)
  if n != len(y):
    print("x and y are of unequal length")
    return "invalid input"
  else:
    # 1. creat design and scatter matrices
    D2 = np.column_stack((x**2, y**2))
    D1 = np.column_stack((x*y, x, y, np.ones(n)))
    S22 = D2.T.dot(D2)
    S21 = D2.T.dot(D1)
    S11 = D1.T.dot(D1)
    M0 = np.linalg.inv(S11).dot(S21.T)
    M = S22 - S21.dot(M0)
    # 2. define the constraint matrix
    C1 = np.array([[0, -2], [-2, 0]])
    # 3. solve the Eigenvalue problem
    e = np.linalg.eig(C1.dot(M))
    lam = e[0]
    i = np.where(lam > 0, lam, np.inf).argmin()
    z2 = e[1][:,i]
    z1 = -M0.dot(z2)
    z = np.concatenate([z2, z1])
    z = z/z[1]
    return z


# SVI equation -----------------------------------------------------------------
def svi(x,z):
  # (i) returns the implied variance for a set of log-moneyness (x) and conic coefficients (z)
  tmp_a = z[2]*x + z[4]
  tmp_b = z[0]*x**2 + z[3]*x + z[5]
  w = (-tmp_a + np.sqrt(tmp_a**2 - 4*z[1]*tmp_b))/(2*z[1])
  return w
