public class Day9 {

    static class Node {
        Node prev;
        Node next;
        long value;
        Node init(Node prev, Node next, long value) {
            this.prev = prev;
            this.next = next;
            this.value = value;
            return this;
        }

        Node prev7() {
            return this.prev.prev.prev.prev.prev.prev.prev;
        } 
    }

    static class DList {
        Node buf[];
        int size = 0;

        DList(int capacity) {
            buf = new Node[capacity];
            for (int i = 0; i < capacity; i++) {
                buf[i] = new Node();
            }
        }

        Node allocate() {
            return buf[size++];
        }

        static List<Long> iterate(Node node) {
            List<Long> values = new ArrayList<>();
            Node current = node;
            do {
                values.add(current.value);
                current = current.next;
            } while (current != node);
            return values;
        }
    }

    static void run(int playerCount, int moveCount) {
        DList dl = new DList(moveCount); 
        Node current = dl.allocate();   
        current.value = 0;
        current.prev = current;
        current.next = current;
        long[] winnings = new long[playerCount];

        for (int marbleNo = 1; marbleNo < moveCount; marbleNo++ ) {
            if (marbleNo % 23 == 0) {
                Node dropped = current.prev7();
                winnings[marbleNo % playerCount] += marbleNo + dropped.value;
                current = dropped.next;
                dropped.prev.next = current;
                current.prev = dropped.prev;
            } else {
                Node before = current.next;
                Node node = dl.allocate().init(before, before.next, marbleNo);
                before.next = node;
                node.next.prev = node;
                current = node;
            } 
        }

        Arrays.sort(winnings);   
        System.out.println(winnings[playerCount - 1]);
        // System.out.println(DList.iterate(current));
    }
}



