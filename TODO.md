Play around with `x[..., 0]` and friends.

```python
f = lambda x: np.sin(10*x[..., 0]) * np.exp(-x[..., 0]**2)
observed_index_points = np.expand_dims(np.random.uniform(-1., 1., 50), -1)
observed_values = f(observed_index_points)
```
