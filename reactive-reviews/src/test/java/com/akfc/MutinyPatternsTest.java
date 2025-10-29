package com.akfc;

import io.quarkus.test.junit.QuarkusTest;
import io.smallrye.mutiny.Multi;
import io.smallrye.mutiny.Uni;
import io.smallrye.mutiny.helpers.test.AssertSubscriber;
import io.smallrye.mutiny.helpers.test.UniAssertSubscriber;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Comprehensive demonstration of Mutiny testing patterns.
 *
 * This test class illustrates 20+ essential patterns for testing
 * reactive code with Mutiny in Quarkus.
 */
@QuarkusTest
@DisplayName("Mutiny Testing Patterns")
public class MutinyPatternsTest {

    // ==================== PATTERN 1: Basic Uni Success ====================

    @Test
    @DisplayName("Pattern 1: Test Uni success with UniAssertSubscriber")
    void pattern01_uniSuccess() {
        // Create a Uni that emits a value
        Uni<String> uni = Uni.createFrom().item("Hello Mutiny!");

        // Subscribe and get the result
        String result = uni
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(result).isEqualTo("Hello Mutiny!");
    }

    // ==================== PATTERN 2: Uni with Transformation ====================

    @Test
    @DisplayName("Pattern 2: Test Uni with map transformation")
    void pattern02_uniTransformation() {
        Uni<Integer> uni = Uni.createFrom().item(5)
            .onItem().transform(x -> x * 2);

        Integer result = uni
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(result).isEqualTo(10);
    }

    // ==================== PATTERN 3: Uni Chaining ====================

    @Test
    @DisplayName("Pattern 3: Test Uni chaining with chain()")
    void pattern03_uniChaining() {
        Uni<String> uni = Uni.createFrom().item(5)
            .onItem().transformToUni(x -> Uni.createFrom().item(x * 2))
            .onItem().transformToUni(x -> Uni.createFrom().item("Result: " + x));

        String result = uni
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(result).isEqualTo("Result: 10");
    }

    // ==================== PATTERN 4: Testing Uni Failure ====================

    @Test
    @DisplayName("Pattern 4: Test Uni failure")
    void pattern04_uniFailure() {
        Uni<String> uni = Uni.createFrom()
            .failure(new IllegalArgumentException("Invalid input"));

        Throwable failure = uni
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitFailure()
            .getFailure();

        assertThat(failure)
            .isInstanceOf(IllegalArgumentException.class)
            .hasMessageContaining("Invalid input");
    }

    // ==================== PATTERN 5: Uni with Timeout ====================

    @Test
    @DisplayName("Pattern 5: Test Uni completes within timeout")
    void pattern05_uniTimeout() {
        Uni<String> uni = Uni.createFrom().item("fast");

        String result = uni
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem(Duration.ofSeconds(1))
            .getItem();

        assertThat(result).isEqualTo("fast");
    }

    // ==================== PATTERN 6: Uni with Delay ====================

    @Test
    @DisplayName("Pattern 6: Test Uni with delayed emission")
    void pattern06_uniDelay() {
        Uni<String> uni = Uni.createFrom().item("delayed")
            .onItem().delayIt().by(Duration.ofMillis(100));

        long start = System.currentTimeMillis();
        String result = uni
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem(Duration.ofSeconds(1))
            .getItem();
        long duration = System.currentTimeMillis() - start;

        assertThat(result).isEqualTo("delayed");
        assertThat(duration).isGreaterThanOrEqualTo(100);
    }

    // ==================== PATTERN 7: Uni Null Handling ====================

    @Test
    @DisplayName("Pattern 7: Test Uni with null item")
    void pattern07_uniNull() {
        Uni<String> uni = Uni.createFrom().nullItem();

        String result = uni
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(result).isNull();
    }

    // ==================== PATTERN 8: Uni<Void> ====================

    @Test
    @DisplayName("Pattern 8: Test Uni<Void> completion")
    void pattern08_uniVoid() {
        Uni<Void> uni = Uni.createFrom().voidItem();

        uni.subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .assertCompleted();
    }

    // ==================== PATTERN 9: Combining Multiple Unis ====================

    @Test
    @DisplayName("Pattern 9: Test combining multiple Unis")
    void pattern09_combineUnis() {
        Uni<Integer> uni1 = Uni.createFrom().item(10);
        Uni<Integer> uni2 = Uni.createFrom().item(20);
        Uni<Integer> uni3 = Uni.createFrom().item(30);

        Uni<Integer> combined = Uni.combine().all().unis(uni1, uni2, uni3)
            .with((a, b, c) -> (Integer) a + (Integer) b + (Integer) c);

        Integer result = combined
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(result).isEqualTo(60);
    }

    // ==================== PATTERN 10: Error Recovery ====================

    @Test
    @DisplayName("Pattern 10: Test Uni error recovery")
    void pattern10_errorRecovery() {
        Uni<String> uni = Uni.createFrom()
            .<String>failure(new RuntimeException("Error"))
            .onFailure().recoverWithItem("fallback");

        String result = uni
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(result).isEqualTo("fallback");
    }

    // ==================== PATTERN 11: Retry on Failure ====================

    @Test
    @DisplayName("Pattern 11: Test Uni retry mechanism")
    void pattern11_retry() {
        AtomicInteger attempts = new AtomicInteger(0);

        Uni<String> uni = Uni.createFrom().deferred(() -> {
            int attempt = attempts.incrementAndGet();
            if (attempt < 3) {
                return Uni.createFrom().failure(new RuntimeException("Attempt " + attempt));
            }
            return Uni.createFrom().item("Success on attempt " + attempt);
        }).onFailure().retry().atMost(5);

        String result = uni
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(result).isEqualTo("Success on attempt 3");
        assertThat(attempts.get()).isEqualTo(3);
    }

    // ==================== PATTERN 12: Multi - Basic Items ====================

    @Test
    @DisplayName("Pattern 12: Test Multi with specific items")
    void pattern12_multiBasic() {
        Multi<Integer> multi = Multi.createFrom().items(1, 2, 3, 4, 5);

        List<Integer> results = multi
            .subscribe()
            .withSubscriber(AssertSubscriber.create(5))
            .awaitCompletion()
            .getItems();

        assertThat(results).containsExactly(1, 2, 3, 4, 5);
    }

    // ==================== PATTERN 13: Multi Transformation ====================

    @Test
    @DisplayName("Pattern 13: Test Multi with map transformation")
    void pattern13_multiTransformation() {
        Multi<Integer> multi = Multi.createFrom().items(1, 2, 3, 4, 5)
            .onItem().transform(x -> x * x);

        List<Integer> results = multi
            .subscribe()
            .withSubscriber(AssertSubscriber.create(5))
            .awaitCompletion()
            .getItems();

        assertThat(results).containsExactly(1, 4, 9, 16, 25);
    }

    // ==================== PATTERN 14: Multi Filtering ====================

    @Test
    @DisplayName("Pattern 14: Test Multi with filter")
    void pattern14_multiFilter() {
        Multi<Integer> multi = Multi.createFrom().items(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
            .select().where(x -> x % 2 == 0);

        List<Integer> results = multi
            .subscribe()
            .withSubscriber(AssertSubscriber.create(Long.MAX_VALUE))
            .awaitCompletion()
            .getItems();

        assertThat(results).containsExactly(2, 4, 6, 8, 10);
    }

    // ==================== PATTERN 15: Multi to Uni ====================

    @Test
    @DisplayName("Pattern 15: Test converting Multi to Uni (collect as list)")
    void pattern15_multiToUni() {
        Multi<Integer> multi = Multi.createFrom().items(1, 2, 3, 4, 5);

        Uni<List<Integer>> uni = multi.collect().asList();

        List<Integer> result = uni
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(result).containsExactly(1, 2, 3, 4, 5);
    }

    // ==================== PATTERN 16: Multi Range ====================

    @Test
    @DisplayName("Pattern 16: Test Multi with range")
    void pattern16_multiRange() {
        Multi<Integer> multi = Multi.createFrom().range(1, 6);

        List<Integer> results = multi
            .subscribe()
            .withSubscriber(AssertSubscriber.create(Long.MAX_VALUE))
            .awaitCompletion()
            .getItems();

        assertThat(results).containsExactly(1, 2, 3, 4, 5);
    }

    // ==================== PATTERN 17: Conditional Logic ====================

    @Test
    @DisplayName("Pattern 17: Test Uni with conditional logic")
    void pattern17_conditionalLogic() {
        Uni<String> uni = Uni.createFrom().item(42)
            .onItem().transform(value ->
                value > 50 ? "high" : "low"
            );

        String result = uni
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(result).isEqualTo("low");
    }

    // ==================== PATTERN 18: Uni ifNull ====================

    @Test
    @DisplayName("Pattern 18: Test Uni ifNull continuation")
    void pattern18_ifNull() {
        Uni<String> uni = Uni.createFrom().<String>nullItem()
            .onItem().ifNull().continueWith("default");

        String result = uni
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(result).isEqualTo("default");
    }

    // ==================== PATTERN 19: Multi Failure ====================

    @Test
    @DisplayName("Pattern 19: Test Multi failure")
    void pattern19_multiFailure() {
        Multi<String> multi = Multi.createFrom()
            .failure(new RuntimeException("Multi failed"));

        Throwable failure = multi
            .subscribe()
            .withSubscriber(AssertSubscriber.create(Long.MAX_VALUE))
            .awaitFailure()
            .getFailure();

        assertThat(failure)
            .isInstanceOf(RuntimeException.class)
            .hasMessageContaining("Multi failed");
    }

    // ==================== PATTERN 20: Complex Pipeline ====================

    @Test
    @DisplayName("Pattern 20: Test complex reactive pipeline")
    void pattern20_complexPipeline() {
        Uni<Integer> pipeline = Uni.createFrom().item(5)
            .onItem().transform(x -> x * 2)           // 10
            .onItem().transformToUni(x -> Uni.createFrom().item(x + 5)) // 15
            .onItem().transform(x -> x * 3)           // 45
            .onFailure().recoverWithItem(0);          // Fallback

        Integer result = pipeline
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(result).isEqualTo(45);
    }

    // ==================== PATTERN 21: Multi from Iterable ====================

    @Test
    @DisplayName("Pattern 21: Test Multi from iterable")
    void pattern21_multiFromIterable() {
        List<String> words = Arrays.asList("Hello", "Mutiny", "World");

        Multi<String> multi = Multi.createFrom().iterable(words);

        List<String> results = multi
            .subscribe()
            .withSubscriber(AssertSubscriber.create(Long.MAX_VALUE))
            .awaitCompletion()
            .getItems();

        assertThat(results).containsExactly("Hello", "Mutiny", "World");
    }

    // ==================== PATTERN 22: Subscription Verification ====================

    @Test
    @DisplayName("Pattern 22: Test subscription lifecycle")
    void pattern22_subscriptionLifecycle() {
        Uni<String> uni = Uni.createFrom().item("test");

        UniAssertSubscriber<String> subscriber = uni
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create());

        // Verify subscription happened
        subscriber.assertSubscribed();

        // Get the result
        String result = subscriber.awaitItem().getItem();

        // Verify completion
        subscriber.assertCompleted();

        assertThat(result).isEqualTo("test");
    }

    // ==================== PATTERN 23: Timeout Failure ====================

    @Test
    @DisplayName("Pattern 23: Test Uni timeout failure")
    void pattern23_timeoutFailure() {
        Uni<String> uni = Uni.createFrom().item("delayed")
            .onItem().delayIt().by(Duration.ofSeconds(5))
            .ifNoItem().after(Duration.ofMillis(100))
            .fail();

        Throwable failure = uni
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitFailure(Duration.ofSeconds(1))
            .getFailure();

        assertThat(failure).isNotNull();
    }

    // ==================== PATTERN 24: Multi Skip and Limit ====================

    @Test
    @DisplayName("Pattern 24: Test Multi skip and limit (pagination)")
    void pattern24_multiPagination() {
        Multi<Integer> multi = Multi.createFrom().range(1, 11)
            .skip().first(3)
            .select().first(3);

        List<Integer> results = multi
            .subscribe()
            .withSubscriber(AssertSubscriber.create(Long.MAX_VALUE))
            .awaitCompletion()
            .getItems();

        assertThat(results).containsExactly(4, 5, 6);
    }

    // ==================== PATTERN 25: Uni Memoization ====================

    @Test
    @DisplayName("Pattern 25: Test Uni memoization (caching)")
    void pattern25_memoization() {
        AtomicInteger counter = new AtomicInteger(0);

        Uni<Integer> uni = Uni.createFrom().item(() -> counter.incrementAndGet())
            .memoize().indefinitely();

        // First subscription
        Integer result1 = uni
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        // Second subscription - should use cached value
        Integer result2 = uni
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(result1).isEqualTo(1);
        assertThat(result2).isEqualTo(1); // Same as result1, not 2
        assertThat(counter.get()).isEqualTo(1); // Only called once
    }
}
